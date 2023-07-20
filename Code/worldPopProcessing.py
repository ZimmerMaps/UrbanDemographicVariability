import rioxarray
from rasterio import features
import xarray as xr
from xrspatial import zonal_stats
from pathlib import Path


def extractInfoToCSV(file, csvTargetFolder, geom, year, age, sex):
    #open raster files and squeeze
    raster_file = rioxarray.open_rasterio(file)
    raster_file_sq = raster_file.squeeze()

    #crop to the right shape
    cropped_raster = features.rasterize(geom, 
                                        out_shape=raster_file_sq.shape, 
                                        fill=0, 
                                        transform=raster_file_sq.rio.transform())

    #change raster to an xarray
    cropped_raster_xarr = xr.DataArray(cropped_raster)

    #run zonal statistics (sum of pixels within polygon)
    raster_df = zonal_stats(zones = cropped_raster_xarr, 
            values = raster_file_sq, 
            nodata_values =  raster_file.rio.nodata, 
            stats_funcs = ['sum'])

    #create new columns in dataframe with year, age and sex
    raster_df['year'] = year
    raster_df['age'] = age
    raster_df['sex'] = sex

    #re-order columns
    raster_df = raster_df[['zone', 'year', 'age', 'sex', 'sum']]

    #save file as csv, use year, sex and age to name files
    raster_df.to_csv(csvTargetFolder / ("df_" + str(year) + "_" + sex + "_" + str(age) + ".csv"))

