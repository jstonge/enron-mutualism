import pandas as pd
import numpy as np
import sys, os
from pathlib import Path

ROOT_DIR = Path("socsemics-enron.Rproj").resolve().parent.parent

# create dat dir, if not already there
if os.path.exists(ROOT_DIR/"dat") is False: os.mkdir(ROOT_DIR/"dat")

#### enron data downloaded from http://www.ahschulz.de/enron-email-data/ ####

################################
##### LOADING PROCEDURES #######
################################

    
def load_employee_list():
    return pd.DataFrame(pd.read_pickle(ROOT_DIR/'raw-data'/'employee_list.pkl'))

def load_message():
    return pd.DataFrame(pd.read_pickle(ROOT_DIR/'raw-data'/'message.pkl'))

def load_recipient_info():
    return pd.DataFrame(pd.read_pickle(ROOT_DIR/'raw-data'/'recipient_info.pkl'))

def load_reference_info():
    return pd.DataFrame(pd.read_pickle(ROOT_DIR/'raw-data'/'reference_info.pkl'))

def load_enron_social_data():
    return pd.DataFrame(pd.read_pickle(ROOT_DIR/'dat'/'enron_social_data.pkl'))

def load_enron_semantic_data():
    return pd.DataFrame(pd.read_pickle(ROOT_DIR/'dat'/'enron_semantic_data.pkl'))

def load_enron_data():
    social_data = load_enron_social_data()
    semantic_data = load_enron_semantic_data()
    enron_data = pd.merge(social_data,
                          semantic_data,
                          on = 'message_id',
                          how = 'left')
    return enron_data

def load_sender_data():
    agents = load_employee_list()
    agents['firstName'] = agents['firstName'].str.lower()
    agents['lastName'] = agents['lastName'].str.lower()
    agents['core_name'] = agents['firstName']+'_'+agents['lastName']
    agents['main_email'] = agents['Email_id']
    agents = agents[['core_name', 'main_email', 'Email_id', 'Email2',
                     'Email3', 'EMail4', 'status']]
    agents = agents.melt(['core_name','status','main_email'],
                         value_name = 'email')
    agents = agents[['core_name', 'status', 'main_email', 'email']]
    agents = agents[agents['email']!=''].sort_values('core_name')
    agents = agents.reset_index(drop = True)
    return agents

def load_sending_data():
    receivers = load_recipient_info()
    receivers = receivers[['mid', 'rtype', 'rvalue']]
    receivers = pd.get_dummies(receivers,
                               columns = ['rtype'],
                               prefix = [''],
                               prefix_sep = '')
    receivers = receivers.rename(columns = {'mid' : 'message_id',
                                            'rtype': 'interaction_type',
                                            'rvalue': 'receiver_email',
                                            'BCC': 'message_BCC_count',
                                            'CC': 'message_CC_count',
                                            'TO': 'message_TO_count'})
    return receivers


######################################
######## CORPUS PROCEDURES ###########
######################################


def filter_corpus(this_corpus):
    #keep only senders that have an enron email or are part of the core
    #reduces dataset from 2 064 442 entries to 1 776 573 entries
    this_corpus = this_corpus[(this_corpus['sender_id'].str.contains('enron')|(this_corpus['is_sender_core']))]
    #keep only receivers that have an enron email or are part of the core
    #reduces dataset to 1 704 835 entries
    this_corpus = this_corpus[(this_corpus['receiver_id'].str.contains('enron')|(this_corpus['is_receiver_core']))]
    #remove messages sent to self
    #reduces dataset to 1 681 026 entries
    this_corpus = this_corpus[(this_corpus['receiver_id'] != this_corpus['sender_id'])]
    return this_corpus

def clean_dates(this_corpus):
    #convert missing values to numpy format
    this_corpus = this_corpus.replace('N/A', np.nan)
    #convert date info to string format to facilitate processing
    this_corpus['date'] = this_corpus['date'].astype(str)
    #correct dates based on enron crisis timeframe
    this_corpus['date'] = this_corpus['date'].str.replace('0002','2002')
    this_corpus['date'] = this_corpus['date'].str.replace('0001','2001')
    this_corpus['date'] = this_corpus['date'].str.replace('1979','1999')
    #convert date info to datetime
    this_corpus['date'] = pd.to_datetime(this_corpus.date)
    #define dates based on enron crisis timeline
    #date enron discloses that it is under investigation by the
    #Securities and Exchange Commission
    sec = '2001-10-31'
    #date enron files for bankrupcy
    bankrupcy = '2001-12-02'
    #date the US Department of Justice open a criminal investigation
    justice = '2002-01-09'
    #assign time periods to messages
    this_corpus['before_sec_announcement']=this_corpus['date'] < sec
    this_corpus['before_bankrupcy']=this_corpus['date'] < bankrupcy
    this_corpus['before_crim_investigation'] = this_corpus['date'] < justice
    return this_corpus

def clean_corpus(this_corpus):
    #convert subject strings to lower case
    this_corpus['subject'] = this_corpus['subject'].str.lower()
    #remove punctuation at end of subject strings
    this_corpus['subject'] = this_corpus['subject'].str.strip('!.?;: ')
    #add period at end of subject strings
    this_corpus['subject'] = this_corpus['subject'] + '.'
    #convert body strings to lower case
    this_corpus['body'] = this_corpus['body'].str.lower()
    #remove punctuation at end of subject strings
    this_corpus['body'] = this_corpus['body'].str.strip('!.?;: ')
    #add period at end of subject strings
    this_corpus['body'] = this_corpus['body'] + '.'
    #create message attribute by merging subject and body strings
    this_corpus['message'] = this_corpus['subject'] + ' ' + this_corpus['body']
    #replace weird encoding characters in first rows
    this_corpus.loc[0:4, 'message'] = this_corpus.loc[0:4, 'message'].str.replace('==20',' ')
    this_corpus.loc[0:4, 'message'] = this_corpus.loc[0:4, 'message'].str.replace('=20',' ')
    this_corpus.loc[0:4, 'message'] = this_corpus.loc[0:4, 'message'].str.replace('=','')
    #delete former subject and body strings
    this_corpus = this_corpus.drop(['subject', 'body'], axis = 1)
    return this_corpus


###########################
#### EXPORT PROCEDURES ####
###########################

def export_social_data(this_corpus):
    #extract and export social data from corpus
    social_data = this_corpus[['message_id', 'date', 'sender_id', 
                               'core_sender_name', 'core_sender_status',
                               'message_BCC_count', 'message_CC_count',
                               'message_TO_count', 'receiver_id', 
                               'core_receiver_name', 'core_receiver_status']]
    social_data = social_data.reset_index(drop = True)
    social_data.to_pickle(ROOT_DIR/'dat'/'enron_social_data.pkl')

def export_semantic_data(this_corpus):
    #extract and export social data from corpus
    semantic_data = this_corpus[['message_id', 'message']]
    #remove duplicated messages
    semantic_data = semantic_data.drop_duplicates()
    semantic_data = semantic_data.reset_index(drop = True)
    semantic_data.to_pickle(ROOT_DIR/'dat'/'enron_semantic_data.pkl')


def main():
    agents = load_sender_data()
    senders = agents.rename(columns = {'core_name': 'core_sender_name',
                                       'main_email': 'sender_main_email',
                                       'email': 'sender_email',
                                       'status': 'core_sender_status'})

    receivers = load_sending_data()
    receivers = pd.merge(receivers, agents,
                         left_on = 'receiver_email',
                         right_on = 'email',
                         how = 'left')
    receivers = receivers.drop(['email'], axis = 1)
    receivers = receivers.rename(columns = {'core_name': 'core_receiver_name',
                                            'status': 'core_receiver_status',
                                            'main_email' : 'receiver_main_email'})
    corpus = load_message()
    corpus = corpus[['mid', 'date', 'sender', 'subject', 'body']]
    corpus = corpus.rename(columns = {'mid': 'message_id',
                                      'sender': 'sender_email'})
    corpus = pd.merge(corpus, senders,
                      on = 'sender_email',
                      how = 'left')
    corpus = pd.merge(corpus, receivers,
                      on = 'message_id',
                      how = 'left')
    #create unique mail attribute for both core and non-core senders
    corpus.loc[corpus['sender_main_email'].notnull(), 'sender_id'] = corpus.loc[corpus['sender_main_email'].notnull(),'sender_main_email']
    corpus.loc[corpus['sender_main_email'].isnull(), 'sender_id'] = corpus.loc[corpus['sender_main_email'].isnull(),'sender_email']
    corpus = corpus.drop(['sender_main_email',
                          'sender_email'], axis = 1)
    #create unique mail attribute for both core and non-core receivers
    corpus.loc[corpus['receiver_main_email'].notnull(), 'receiver_id'] = corpus.loc[corpus['receiver_main_email'].notnull(),'receiver_main_email']
    corpus.loc[corpus['receiver_main_email'].isnull(), 'receiver_id'] = corpus.loc[corpus['receiver_main_email'].isnull(),'receiver_email']
    corpus = corpus.drop(['receiver_main_email',
                                    'receiver_email'], axis = 1)
    #create core attribute
    corpus['is_sender_core'] = corpus['core_sender_name'].notnull()
    corpus['is_receiver_core'] = corpus['core_receiver_name'].notnull()
    corpus = filter_corpus(corpus)
    corpus = clean_dates(corpus)
    corpus = clean_corpus(corpus)
    export_social_data(corpus)
    export_semantic_data(corpus)
    return corpus

if __name__ == '__main__':
    main()
