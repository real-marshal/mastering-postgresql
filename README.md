#### This repo contains different SQL queries I made reading the book _'Art of PostgreSQL'_ on `f1db` DB from http://ergast.com/mrd/db/.

Usually I try to make the query myself first after reading what the author is going to do, but obviously sometimes I needed to take a look at the solution first to get the idea. Some of the queries were not presented in the book and made completely by me.

#### Setup

I prefer using `docker` for most of the dev staff, so here's how I set up the DB from the MySQL dump from the link above.
1. Install `pgloader` (like `\copy`/`COPY` but with automatic schema generation and better error handling).
2. Start MySQL container `docker run -d -e MYSQL_ROOT_PASSWORD='password' --name mysql mysql`.
3. Create a temp dir for config volume (we need to make some changes due to MySQL 8 new auth method that `pgloader` doesn't support) `mkdir temp`.
4. Extract the default config file `docker exec -it mysql cat /etc/my.cnf > temp/my.cnf`.
5. Add `default-authentication-plugin=mysql_native_password` to `[mysqld]` section.
6. Start new MySQL container with the fixed config `docker run -d -e MYSQL_ROOT_PASSWORD='password' -v ~/temp:/etc/mysql/conf.d --name mysql mysql`.
7. Launch `mysql` CLI `docker exec -it mysql mysql -uroot -ppassword`.
8. Create a new DB `create database f1db;`
9. Restore the dump `gzip -cd ./Downloads/f1db.sql.gz | docker exec -i mysql mysql -uroot -ppassword f1db
   `.
10. Launch `psql` in some PostgreSQL container `docker exec -it <postgresql_container> psql -U postgres`.
11. Create a new DB `create database f1db;`
12. Find IP addresses of the containers `docker inspect mysql | grep IP` and the same for the PostgreSQL container.
13. Load the DB into PostgreSQL container `pgloader mysql://root:password@<ip_address>/f1db postgresql://postgres:password@<ip_address>/f1db`