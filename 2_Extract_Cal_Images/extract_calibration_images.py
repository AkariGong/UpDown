from math import floor
import os, sys
import glob
import re
import shutil

videodir = r'D:\UpAndDown\UpAndDown-Ming-2022-02-22-3d\calibration_images'
outdir = r'D:\UpAndDown\UpAndDown-Ming-2022-02-22-3d\calibration_images'

for f in glob.glob(os.path.join(videodir, 'camera*.jpg')):
    m = re.search('camera-(\w+)-(\d+)', f)

    outname = '{}-{}.jpg'.format(m.group(1), m.group(2))
    shutil.copyfile(f, os.path.join(outdir, outname))