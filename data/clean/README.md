DATASET

The fish_data_clean.csv file contains the entire clean and formatted dataset used for the following project. 

DESCRIPTIVE ABSTRACT:

159 fishes of 7 species are caught and measured. Altogether there are
15 variables.  All the fishes are caught from the same lake
(Laengelmavesi) near Tampere in Finland in 1917.

SOURCES:
Brofeldt, Pekka: Bidrag till kaennedom on fiskbestondet i vaera
        sjoear. Laengelmaevesi. T.H.Jaervi: Finlands Fiskeriet  Band 4,
        Meddelanden utgivna av fiskerifoereningen i Finland.
        Helsingfors 1917

VARIABLE DESCRIPTIONS:

1  Species   (Numeric)
        Code Finnish  Swedish    English        Latin      
         1   Lahna    Braxen     Bream          Abramis brama
         2   Siika    Iiden      Whitewish      Leusiscus idus
         3   Saerki   Moerten    Roach          Leuciscus rutilus
         4   Parkki   Bjoerknan  ?              Abramis bjrkna
         5   Norssi   Norssen    Smelt          Osmerus eperlanus
         6   Hauki    Jaedda     Pike           Esox lucius
         7   Ahven    Abborre    Perch          Perca fluviatilis

2  Weight      Weight of the fish (in grams)
3  Length1     Length from the nose to the beginning of the tail (in cm)
4  Length2     Length from the nose to the notch of the tail (in cm)
5  Length3     Length from the nose to the end of the tail (in cm)
6  Height%     Maximal height as % of Length3
7  Width%      Maximal width as % of Length3
8  Sex         1 = male , 0 = female

(Dummy Variables)

9  Species1    1 = Specie code equals to 1 ,  0 = Any code different than 1
10  Species2    1 = Specie code equals to 2 ,  0 = Any code different than 2
11  Species3    1 = Specie code equals to 3 ,  0 = Any code different than 3
12  Species4    1 = Specie code equals to 4 ,  0 = Any code different than 4
13  Species5    1 = Specie code equals to 5 ,  0 = Any code different than 5
14  Species6    1 = Specie code equals to 6 ,  0 = Any code different than 6
15  Species7    1 = Specie code equals to 7 ,  0 = Any code different than 7


          ___/////___                  _
         /           \    ___          |
       /\             \_ /  /          H
     <   )            __)  \           |
       \/_\\_________/   \__\          _

     |------- L1 -------|
     |------- L2 ----------|
     |------- L3 ------------|


Values are aligned and delimited by blanks.
Missing values are denoted with NA.
There is one data line for each case.

EXCLUDED OBSERVATIONS:

Two values that couldn't be used on our dependent variable weight (rows 14 and 47). 
One the value corresponds to 0 (It doesn't make any sense to have any fish size equals to 0). 
And the other shows as a Missing Value. Both of them are errors in the collection.

So, the project finally ended up having 157 observations for analysis.

