
import pandas as pd

#Data management libraries
import sqlite3

connection = sqlite3.connect("database_17_0.sqlite")
df = pd.read_sql_query("SELECT * from data", connection)
connection.close()

# select columns if a candidate is mentioned
df1 = df.loc[df['mention_Mélenchon']== 1,['text','location','user','day']]
df2=  df.loc[df['mention_Fillon']== 1,['text','location','user','day']]
df3 = df.loc[df['mention_Le Pen']== 1,['text','location','user','day']]
df4 = df.loc[df['mention_Macron']== 1,['text','location','user','day']]
df5 = df.loc[df['mention_Hamon']== 1,['text','location','user','day']]

# examples of converting columns in df to lists
text1 = df1['text'].tolist()
text2 = df2['text'].tolist()
location3 = df3['location'].tolist()
user4 = df4['user'].tolist()
day5 = df5['day'].tolist()

print(user4)
