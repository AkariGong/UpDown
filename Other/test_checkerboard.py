import cv2
import os

testfile = '/Users/etytel01/Documents/2022/UpAndDown/UpAndDown-Eric-2022-02-10-3d/removed_calibration_images/camera-rear-005.jpg'
outdir = '/Users/etytel01/Documents/2022/UpAndDown/UpAndDown-Eric-2022-02-10-3d/corners'

img = cv2.imread(testfile)
gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)

pn, fn = os.path.split(testfile)

print('{}'.format(fn))
ret, corners = cv2.findChessboardCorners(gray, (7,7), cv2.CALIB_CB_ADAPTIVE_THRESH + \
    cv2.CALIB_CB_FAST_CHECK + cv2.CALIB_CB_NORMALIZE_IMAGE)

if ret:
    print('Found!')
else:
    print('Not found')

img = cv2.drawChessboardCorners(img, (7,7), corners, ret)

cv2.imwrite(os.path.join(outdir, fn), img)
