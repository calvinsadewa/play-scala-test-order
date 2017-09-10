# --- first database schema

# --- !Ups

CREATE TABLE coupon_users (
    coupon_id int NOT NULL,
    user_id int NOT NULL,
    amount int NOT NULL,
    PRIMARY KEY (coupon_id,user_id)
);

CREATE TABLE coupons (
    id int NOT NULL,
    portion_cut decimal NOT NULL,
    nominal_cut decimal NOT NULL,
    valid_start bigint NOT NULL,
    valid_end bigint NOT NULL,
    PRIMARY KEY (id)
);

CREATE TABLE products (
    id int NOT NULL,
    amount int NOT NULL,
    price_per decimal NOT NULL,
    PRIMARY KEY (id)
);

# --- !Downs

DROP TABLE coupon_users;
DROP TABLE coupons;
DROP TABLE products