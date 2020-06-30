## title: read in data on moves
## date: 27.03
## author: trond

## house keeping
import cbsodata
import pandas

## read catalogus
cbs_dt = 

## read data
mig_data = pandas.DataFrame(cbsodata.get_data('60048ned'))

test = pandas.DataFrame(cbsodata.get_data('60048ned', filters="RegioS eq 'GM0363'"))

mig_data[mig_data['RegioS'] == 'Amsterdam']

## show column names
mig_data[mig_data['RegioS'] == 'Nederland']

info = cbsodata.get_info('60048ned')

## show column names
list(mig_data.columns.values)

## show unique regions
test['ID'].unique()

mig_data['RegioS'].unique()





