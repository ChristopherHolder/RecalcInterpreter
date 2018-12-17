#!/bin/bash
for i in {0..5}
do
    ./proj2_linprog < "$i.error"
done
