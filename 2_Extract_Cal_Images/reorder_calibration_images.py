from math import floor
import os, sys
import glob
import re
import shutil

# change these so that they have the right directory names
videodir = r'/Users/minggong/Documents/Tytell_Lab/Project2/Data/Calibration_Images/all_images/20220729/rear01'
outdir = r'/Users/minggong/Documents/Tytell_Lab/Project2/Data/Calibration_Images/cal_images/20220729/rear01'

# take every 50th frame
step = 20

for f in glob.glob(os.path.join(videodir, '*.jpg')):
    _, fn = os.path.split(f)
    m = re.search('(.+)-(\d+)', fn)

    fr = int(m.group(2))
    if fr % step == 0:
        frout = floor(fr / step)

        outname = '{}-{:03d}.jpg'.format(m.group(1), frout)
        print(outname)

        shutil.copyfile(f, os.path.join(outdir, outname))