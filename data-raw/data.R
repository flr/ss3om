# data.R - DESC
# ss3om/data-raw/data.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(ss3om)

# SOL

sol <- readOutputss3('sol')

save(sol, file='../data/sol.rda', compress='xz')

# SMA

sma <- readOutputss3('sma')

save(sma, file='../data/sma.rda', compress='xz')
