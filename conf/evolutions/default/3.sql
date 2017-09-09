# --- first database schema

# --- !Ups

CREATE TABLE ORDERINBROWSE (
    user_id int NOT NULL,
    order_data text NOT NULL,
    version int NOT NULL,
    coupon_id int
    PRIMARY KEY (user_id)
);

# --- !Downs

DROP TABLE ORDERINBROWSE;