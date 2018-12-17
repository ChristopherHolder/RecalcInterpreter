#!/bin/bash
for i in {0..9}
do
    ./proj2_linprog < "$i.test"
done
