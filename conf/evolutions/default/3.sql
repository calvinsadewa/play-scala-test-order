# --- first database schema

# --- !Ups

CREATE TABLE submitted_order (
    id serial NOT NULL,
    user_id int NOT NULL,
    order_data text NOT NULL,
    coupon_id int,
    profile text NOT NULL,
    total_price numeric NOT NULL,
    verified boolean NOT NULL,
    payment_proof int,
    shipping_id int,
    PRIMARY KEY (id)
);

# --- !Downs

DROP TABLE submitted_order;