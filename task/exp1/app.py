from flask import Flask, render_template, url_for, request, jsonify
import pandas as pd
import sqlalchemy as sql
from json import load
from time import sleep

app = Flask(__name__)

DATABASE_URL = load(open('dbconfig.txt'))['DATABASE_URL']
DATABASE_URL = DATABASE_URL.replace('postgres', 'postgresql')

@app.route('/')
def index():
  return render_template('index.html')

@app.route('/save_data', methods=['POST','GET'])
def save_data():
  # Get subject data
  all_json  = request.get_json()

  # Parse with pandas
  dfs = []
  for i,json in enumerate(all_json):

    df = pd.DataFrame(json, index=[0])
    dfs.append(df)

  data  = pd.concat(dfs)

  # Save data to db in json format
  engine = sql.create_engine(DATABASE_URL)
  conn   = engine.connect()
  idx    = 'subj_' + data.subject.iloc[0] 
  j      = data.reset_index().to_json()
  entry  = pd.DataFrame({'subject': [idx], 'datastring':[j]})
  
  entry.to_sql(idx, conn, if_exists='replace')
  conn.close()

  return jsonify({'data_processed':'true'})

if __name__ == "__main__":
  app.run()
