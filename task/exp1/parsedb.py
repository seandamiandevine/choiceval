import pandas as pd
import sqlalchemy as sql
from json import load
from datetime import datetime
import os

# os.chdir('/Users/sean/documents/ChoiceVal/v5/task')

DATABASE_URL = load(open('dbconfig.txt'))['DATABASE_URL']
DATABASE_URL = DATABASE_URL.replace('postgres', 'postgresql')

engine = sql.create_engine(DATABASE_URL)
tables = sql.inspect(engine).get_table_names()

min_date = datetime(2022, 9, 16) # data before this will not be saved 

for t in tables:
	try: 
		sub   = pd.read_sql(f"SELECT * FROM {t}", engine)
		sub   = pd.read_json(sub.datastring.iloc[0]) # read json
		fname = f'data/CHOICEVAL_{sub.subject.iloc[0]}_{sub.date.iloc[0]}.csv'
		dt    = pd.to_datetime(sub.date.iloc[0])#.tz_localize('est')

		if dt >= min_date:			
			print(f'processed data for PROL ID: {sub.prol_id.iloc[0]}')
			sub.to_csv(fname)
			os.utime(fname, (dt.timestamp(), dt.timestamp() ))

	except:
		print(f'problem reading table {t}')

print(f"Number of subjects = {len([i for i in os.listdir('data/') if '.DS' not in i])}")


