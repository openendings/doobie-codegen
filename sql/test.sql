-- Types mostly lifted from 
-- http://www.postgresql.org/docs/9.4/static/datatype.html

CREATE TABLE test_types (
    bi   BIGINT,
    bo   BOOLEAN,
    dou  DOUBLE PRECISION,
    inte INTEGER,
    te   TEXT,
    ts   TIMESTAMP,
    tsz  TIMESTAMP WITH TIME ZONE
);

CREATE SCHEMA schema;

CREATE TABLE schema.test (
    id BIGINT
);
