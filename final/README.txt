{- 
  File      :   Main.hs, Parsing.hs, Drawing.hs, SimLogic.hs, DataTypes.hs 
  Copyright :   (c) Gianni Chen, 12/04/17
  Notes		:	I worked together with Andrew Mao for some parts of the assignment, so certain parts may look similar
  Contents  :   Schelling model + some extra credit stuff
-}

Compiling Instructions:
- ghc Main.hs
- There is also a makefile, just substitute the "stack ghc" part with your own ghc compiler
- Then run ./Main maxRounds [-f fileName]
	- Where maxRounds is the maximum number of simulation steps, and fileName is the name of the file
- You can test the -f option with grid.txt that was included in the folder

Contents:
- Main.hs: 		Main entry point of the program that checks for arguments, handles user input, and calls the step function
- Parsing.hs:	Mainly deals with parsing the contents of the file if passed as argument and inits the sim state
- Drawing.hs:	Hosts the main function that draws a the picture given the arguments and the sim state
- SimLogic.hs:	Main simulation logic that gets run at every simulation step
- DataTypes.hs: Hosts the data types that are used throughout the program

User Controls:
- [Q] [W]:			Decreases/Increases the similarity threshold by 5%, does not reset simulation
- [A] [S]:			Decreases/Increases the neighborhood radius by 1
- [Z] [X]:			Decreases/Increases the simulation delay by 100ms, does not reset simulation
- [R] [T]:			Decreases/Increases the red-to-blue ratio by 5%
- [F] [G]:			Decreases/Increases the empty percentage by 5%
- [V] [B]:			Decreases/Increases the parks percentage by 5%
- [U] [I] [O] [P]:	Decreases/Increases the size of the map by 1x1 or 5x5

- [Enter]:			Start/Stop the simulation
- [Left]:			Reset the simulation
- [Right]:			Step through one simulation step
- [ESC]:			Exits the simulation (it simply returns an error...)

Extra Credits Content:

Extra Instances

- Instances of Functor, Applicative, Monad, Monoid, and Foldable are implemented on the following data types:
	- City
	- Row
	- Neighborhood
	- OpenHouse
- However, not all the instances were applied

Parks

- Parks were included in the simulation model, and they are represented as green squares
- The number of parks is determined first, then the number of empty house slots, and finally the red/blue houses
	- For example, with 10% parks, 20% empty, and 50/50 split of red to blue on a 100 square map, we will get the following:
		- round(100 * 0.1) = 10 parks, 90 houses
		- round(90 * 0.2) = 18 empty houses, 72 non-empty houses
		- round(72 * 0.5) = 36 red houses, 36 blue houses
- Once spawned, parks never move or swap locations since they are parks
- In comparing similarity, a park is considered of the same color as the comparing color
- This is because everyone likes parks, and it can be the deciding factor between a satisfied/unsatisfied household both in the simulation and in real life
- In some cases, you will see that a small cluster of colors will still cling together because of the presence of parks
- The higher the concentration of parks there are in the simulation, the less likely you will see gentrified clusters