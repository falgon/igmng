create database if not exists igmng;
grant all on igmng.* to roki@'%';
use igmng;

create table if not exists follow_log (
    id int(11) unsigned auto_increment not null comment "log id",
    followees_num int(11) unsigned not null comment "followees_num",
    followers_num int(11) unsigned not null comment "followes_num",
    logged_date datetime not null comment "logged datetime",
    followers text not null comment "followers",
    primary key (id),
    index idx_logged_date(logged_date)
) engine=InnoDB default charset=utf8;

