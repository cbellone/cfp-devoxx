#!/bin/sh
rm -f conf/application.conf
cp conf/application-vdz.conf conf/application.conf
rm -rf target/
play dist
