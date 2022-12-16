-- top drivers of all time
select format('%s %s', forename, surname) as name, count(*) as wins
from drivers
         left join results using (driverid)
         left join races using (raceid)
where position = 1
group by driverid
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
    group by decade, driverid
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
               count(raceid) as races
        from drivers
                 left join results using(driverid)
                 left join races using(raceid)
        group by driverid
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
