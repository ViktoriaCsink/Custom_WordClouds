# NLP_WordClouds

Extract_Frequencies.R
This script will produce word clouds for each topics you specify.
The plots will not only reflect the frequency of a word, but also whether the words are also common in all/most of the documents in the relevant topic.
To that end, the frequency of each word is calculated in each document, as well as the frequency of the word in the topic as a whole.
Words are selected for plotting if they are frequent in many documents. (See comments in the script for details).

Input:
Documents folder with texts on each topic
To include more topics, add a folder with documents. name the folder 'Topic X'.

Output1:
Relative frequencies 1.csv/2.csv
The table with the word frequencies in each topic (currently 2, but any number of topics are supported).
Num_in_doc: The number of times a word appeared in a document
Prop_in_doc: Num_in_doc/total number of words in the document*100
Num_in_total: The number of times the word appeared in all docs on the topic
Prop_in_total: Num_in_total/total number of words in the corpus*100
Relative frequency: abs(Prop_in_doc-Prop_in_total)
  Small values for relative frequency indicate that the word is similarly frequent in that document AND in the topic as a whole = good keywords
  Extract_Frequencies.R selects words with high frequencies AND small Relative frequency values.
  
Output2:
Topic 1.pdf/Topic 2.pdf
Word clouds in pdf format

