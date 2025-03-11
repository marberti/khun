#! /bin/bash

khun_x=../../khun.x

counter=0
while true
do
  ./random_matrix.x > m
  r1=$(${khun_x} < m)
  r2=$(python3 main.py)

  r1_1=$(echo ${r1} | cut -f 1 -d ":")
  r1_2=$(echo ${r1} | cut -f 2 -d ":")
  r2_1=$(echo ${r2} | cut -f 1 -d ":")
  r2_2=$(echo ${r2} | cut -f 2 -d ":")

  printf "%s %s" $r1 $r2
  if [ $r1_1 != $r2_1 ]; then
    printf " indx"
  fi
  if [ $r1_2 != $r2_2 ]; then
    printf " sum"
    printf "\nPass: %d\n" $counter
    exit 1
  fi
  printf "\n"

  counter=$(echo $counter + 1 | bc -l)
  if [ $(($counter % 100)) == 0 ]; then
    printf "Pass: %d\n" $counter
  fi
done
