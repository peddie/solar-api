#!/usr/bin/env bash

R --slave -e "options(browser = 'firefox'); source('fetch_solar.R')"
