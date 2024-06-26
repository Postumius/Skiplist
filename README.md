# Skiplist
https://en.wikipedia.org/wiki/Skip_list

Basically, it's a search tree (or at least my implementation is) with a randomized structure.

## Usage
Inside of ```Skiplist/```, run ```cabal repl Skiplist.hs```

```testOnRandoms n``` makes a list of length ```n``` filled with random numbers from 1-99, and converts it to a skiplist
```haskell
testOnRandoms 40
[20,4,16,58,91,36,77,9,88,21,75,86,41,97,26,0,28,26,77,63,92,55,98,23,37,39,16,57,94,20,43,18,90,69,1,21,1,35,88,36]

 |____________________________________________43                                          
 |________9___________________________37      |                                           
 |__0     |__________21____________36 |       |__55____________________88                 
 |  |     |__16___20 |             |  |       |  |__________69______86 |__________94_97   
 |_ |_1_4 |_ |_18 |_ |_23_26_28_35 |_ |_39_41 |_ |_57_58_63 |_75_77 |_ |_90_91_92 |_ |_98 
```

```testOnList``` takes a list and converts it to a skiplist
```haskell
testOnList [1..40]
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40]

 |______________________________________________________________________________________33                   
 |__1                                                                                   |                    
 |  |___________5__________10________14                                                 |_______36______39   
 |  |__2__3__4  |___7      |____12   |_____________________22___________________30      |       |       |    
 |  |  |  |  |  |   |__8   |    |    |__________18______21 |_______25___27      |__31   |       |       |    
 |_ |_ |_ |_ |_ |_6 |_ |_9 |_11 |_13 |_15_16_17 |_19_20 |_ |_23_24 |_26 |_28_29 |_ |_32 |_34_35 |_37_38 |_40 
```
