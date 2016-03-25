-- Types mostly lifted from 
-- http://www.postgresql.org/docs/9.4/static/datatype.html

CREATE TABLE test_types (
    bigint_t     BIGINT,
    boolean_t    BOOLEAN,
    double_t     DOUBLE PRECISION,
    integer_t    INTEGER,
    text_t       TEXT,
    timestamp_t  TIMESTAMP,
    timestampz_t TIMESTAMP WITH TIME ZONE
);

CREATE SCHEMA schema;

CREATE TABLE schema.test (
    id BIGINT
);

CREATE TABLE test_nullible (
    always_string    TEXT NOT NULL,
    sometimes_string TEXT
);

CREATE TABLE test_pk (
    id BIGINT PRIMARY KEY
);

CREATE TABLE test_composite_pk (
    id1 BIGINT,
    id2 BIGINT,
    PRIMARY KEY(id1, id2)
);
