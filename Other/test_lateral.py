#!/usr/bin/env python3

import os
import csv
import pandas

ms03_20211013_trial01 = open('/Users/minggong/Documents/Tytell_Lab/Project2/Data/UpAndDownLateral3/videos/20211013_ms03_trial01_lateralDLC_resnet50_UpAndDownLateral2Feb7shuffle1_1030000.csv')

type(ms03_20211013_trial01)

csvreader = csv.reader(ms03_20211013_trial01)

header = []

header = next(csvreader)

# print(header)

ms03_20211013_trial02 = pandas.read_csv("/Users/minggong/Documents/Tytell_Lab/Project2/Data/UpAndDownLateral3/videos/20211013_ms03_trial02_lateralDLC_resnet50_UpAndDownLateral2Feb7shuffle1_1030000.csv")

