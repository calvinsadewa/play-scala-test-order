# --- first database schema

# --- !Ups

ALTER TABLE submitted_order ADD canceled BOOLEAN NOT NULL DEFAULT '0'

# --- !Downs

ALTER TABLE submitted_order DROP canceled