#!/bin/bash

psql -U postgres -c 'DROP DATABASE gen'
psql -U postgres -c 'CREATE DATABASE gen'
cat test.sql | psql -U postgres gen
