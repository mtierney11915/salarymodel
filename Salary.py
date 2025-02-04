#Import Packages
import streamlit
import streamlit as St
import pandas as pd

#Streamlit Launch
St.title('MLB Free Agency Model Application')
St.subheader('By: Matt Tierney')
St.header('Project Description:')
St.write('This application is designed to be an interactive supplemental component to my MLB Free Agency Modeling project. Using MLB Free Agency Historical Data (2015-2024) sourced from Jeff Eustis at Baseball Prospectus, four seperate models were fit to predict Average Annual Value of a contract (AAV), and how many years the contract would be for both hitters and pitchers. The goal of this project is for it to be a tool for MLB franchises to properly assign a quanitative value to a player. If you have any questions, feel free to reach out to me at matthew.tierney24@gmail.com')
St.divider()
St.header('Choose What Players You Would Like to See Here:')
#Data
old=pd.read_csv('Oldcontracts1.csv')
new=pd.read_csv('FA Preds.csv')
old['Offseason Year'] = old['Offseason Year'].astype(str)
model=pd.read_csv('Model Results.csv')
#Selection
c1=St.selectbox('Would you like to view predicted contracts for the 2024 offseason or view predicted historical contracts?',("2024 Offseason Contracts","Historical Contracts"))
if c1 == "2024 Offseason Contracts":
    newP=streamlit.selectbox('Select or enter which player you would like to view:',new['Player'])
    newplayer = new[new['Player'] == newP]
    St.header('Predicted Contract:')
    St.dataframe(newplayer)
if c1 == "Historical Contracts":
    oldP=streamlit.selectbox('Select or enter which player you would like to view:',old['Player'])
    oldplayer = old[old['Player'] == oldP]
    St.header('Predicted Contract(s):')
    St.dataframe(oldplayer)

#Display Model Results
St.header('Overall Model Performance')
mod=St.selectbox("Are you interestd in learning more about overall model performance?",("No","Yes"))
if mod == "Yes":
    St.dataframe(model)
