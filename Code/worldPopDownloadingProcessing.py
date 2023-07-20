import urllib.request
from pathlib import Path
import worldPopProcessing
import geopandas as gpd 
import os
import multiprocessing
from functools import partial

def getUrl(year, sex, age):
    url = 'https://data.worldpop.org/GIS/AgeSex_structures/Global_2000_2020/' + str(year) + '/0_Mosaicked/global_mosaic_1km/global_' + sex + '_' + str(age) + '_' + str(year) + '_1km.tif'
    return url

def getName(year, sex, age):
    return 'global_' + sex + '_' + str(age) + '_' + str(year) + '_1km.tif'

def getFile(targetFolder, year, sex, age):
    url = getUrl(year, sex, age)
    name = getName(year, sex, age)

    print('starting downloading: ' + name)

    targetFile = targetFolder/name

    if(targetFile.exists()):
        print('file already exists: ' + name)
        return targetFile

    urllib.request.urlretrieve(url, targetFile)

    print('done: ' + name)
    return targetFile

rasterTargetFolder = Path('../Data/Rasters')
csvTargetFolder = Path('../Data/CSVs')

geom = gpd.read_file('/Users/azimmer/Documents/Research/Zimmer - Population Pyramid/Urban Polygons/UCDB/ghs_ucdb.shp').to_crs('epsg:4326')[['geometry', 'fid']].values.tolist()

sexes = ['f','m']
ages = [0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80]
years = range(2002,2020)

year = 2002
# age = 0
# sex = 'f'

def downloadFiles(age, rasterTargetFolder, year, csvTargetFolder, geom):
    for sex in sexes:
        file = getFile(rasterTargetFolder, year, sex, age)
        worldPopProcessing.extractInfoToCSV(file, csvTargetFolder, geom, year, age, sex)
        os.remove(file)

with multiprocessing.Pool(3) as pool:
    pool.map(partial(downloadFiles, rasterTargetFolder=rasterTargetFolder, year=year, csvTargetFolder=csvTargetFolder, geom=geom), ages)


# file = getFile(rasterTargetFolder, year, sex, age)
# worldPopProcessing.extractInfoToCSV(file, csvTargetFolder, geom, year, age, sex)
# os.remove(file)
