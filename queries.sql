-- top drivers of all time
select format('%s %s', forename, surname) as name, count(*) as wins
from drivers
    left join results using (driverid)
    left join races using (raceid)
where position = 1
group by format('%s %s', forename, surname)
order by wins desc;

-- top 3 drivers of a decade
with decades as (
    select extract(year from date_trunc('decade', date)) as decade
    from races
    group by decade
    order by decade
)
select decade, name, wins,
       rank() over (partition by decade order by wins desc)
from decades
    left join lateral (
        select format('%s %s', forename, surname) as name, count(*) as wins
        from drivers
             left join results using (driverid)
             left join races using (raceid)
        where position = 1 and year >= decade and year < decade + 10
        group by decade, format('%s %s', forename, surname)
        order by wins desc
        limit 3
    ) as winners on true
order by decade, wins desc;

-- drivers who never finished a race
select name, races
from (
    select format('%s %s', forename, surname) as name,
        every(position is null) as never_finished,
        count(raceid) as races
    from drivers
        left join results using(driverid)
        left join races using(raceid)
    group by driverid
    order by races desc
) as idk
where never_finished = true;

-- reasons for never completing a race of the guy with
-- the most amount of unfinished races
with
    drivers_with_never_finished_column as (
        select format('%s %s', forename, surname) as name,
               every(position is null) as never_finished,
               -- this filter doesn't change the results (because later we omit the
               -- drivers who never finished anyway) but it improves perf a little
               -- bit as now we count only the drivers we are interested in
               count(raceid) filter (where position is null) as races
        from drivers
            left join results using(driverid)
            left join races using(raceid)
        group by format('%s %s', forename, surname)
        order by races desc
    ),
    top_driver_who_never_finished as (
        select name, races
        from drivers_with_never_finished_column
        where never_finished = true
        order by races desc
        limit 1
    )
select races.name, status, races.date
from results
    left join status using(statusid)
    left join drivers using(driverid)
    left join races using(raceid)
where format('%s %s', forename, surname) =
    (select name from top_driver_who_never_finished)
order by races.date;

-- drivers and constructors points in some year
-- \set only works in psql obviously
\set year 1978;
select format('%s %s', forename, surname) as driver,
       constructors.name as constructor,
       sum(points) as points
from results
    left join drivers using(driverid)
    left join constructors using(constructorid)
    left join races using(raceid)
where races.year = :year
group by grouping sets (
    format('%s %s', forename, surname),
    constructors.name
)
having sum(points) > 20
order by constructors.name is not null,
         format('%s %s', forename, surname) is not null,
         points desc;

-- checking out statuses to find accident status
select * from status;

-- most dangerous years
select year, count(raceid) as accidents
from results
    left join races using(raceid)
    left join status using(statusid)
where statusid = 3
group by year
order by accidents desc
limit 10;

-- percentage of accidents of participating drivers
with race_counts as (
    select year,
        count(raceid) as races,
        count(raceid) filter (where statusid = 3) as accidents
    from results
        left join races using (raceid)
        left join status using (statusid)
    group by year
    order by accidents desc
    limit 10
)
select year,
       round((accidents::real / races::real * 100)::numeric, 2) as "%",
       repeat('=', ceil(accidents::real / races::real * 100)::int) as "% graph",
       races
from race_counts
order by "%" desc;

-- best driver/constructor pair
with
    year_points as (
        select year,
               format('%s %s', forename, surname) as driver,
               constructors.name as constructor,
               sum(points) as points
        from results
            left join races using(raceid)
            left join drivers using(driverid)
            left join constructors using(constructorid)
        group by grouping sets (
            (year, format('%s %s', forename, surname)),
            (year, constructors.name)
        )
    ),
    max_points as (
        select year,
               max(points) filter(where driver is not null) as max_driver_points,
               max(points) filter(where constructor is not null) as max_constructor_points
        from year_points
        group by year
    )
select max_points.year,
       drivers_year_points.driver,
       constructors_year_points.constructor,
       round((max_driver_points + max_constructor_points)::numeric, 2) as points,
       max_driver_points as driver_points,
       max_constructor_points as constructor_points
from max_points
    left join year_points as drivers_year_points on
        max_points.year = drivers_year_points.year and
        max_points.max_driver_points = drivers_year_points.points and
        drivers_year_points.driver is not null
    left join year_points as constructors_year_points on
        max_points.year = constructors_year_points.year and
        max_points.max_constructor_points = constructors_year_points.points and
        constructors_year_points.constructor is not null
order by max_points.year;

-- drivers who won a race at least once
select distinct on(driverid)
    format('%s %s', forename, surname)
from drivers
    left join results using(driverid)
where position = 1;

-- trying out window functions
select x,
       array_agg(x) over (rows between unbounded preceding and current row)
from generate_series(1, 3) as t(x);

-- array constructor with subquery as an alternative to array_agg
select array (
    select x from generate_series(1, 3) as t(x)
);

-- drivers standings in a race coupled with their position
-- relative to other drivers of the same constructor
\set raceid 890;
select format('%s %s', forename, surname),
       constructors.name as name,
       position,
       format(
           '%s/%s',
           -- rank, dense_rank, row_number can be use here without any difference
           -- as there can't be duplicate rows in the result
           row_number() over (partition by constructorid order by position),
           count(*) over (partition by constructorid)
       ) as pos_per_constr
from races
    left join results using(raceid)
    left join drivers using(driverid)
    left join constructors using(constructorid)
where raceid = :raceid
order by position;

-- drivers standings, their fastest lap speed standings, some group (no idea why),
-- previous and next driver codes
\set raceid 890;
select format('%s %s', forename, surname) as driver,
       position,
       row_number() over (order by fastestlapspeed::real) as fastest_lap_speed_pos,
       ntile(4) over (order by position) as "group",
       lag(drivers.code, 1, '<first>') over (order by position) as prev,
       lead(drivers.code, 1, '<last>') over (order by position) as next
from races
    left join results using(raceid)
    left join drivers using(driverid)
where raceid = :raceid
order by position;