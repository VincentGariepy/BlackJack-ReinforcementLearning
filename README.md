# BlackJack-ReinforcementLearning
Here are two approaches to trying to solve the optimal policy of a Black Jack game. 
The Monte Carlo method simulates games of BlackJack with 6 decks and counts the card using the standard card counting method, then tries to see if there is a relationship between the count of cards and the outcome of a game.  

The REINFORCE method, approaches the problem with a random game, meaning there are infinite cards and any can be drawn at every step, so no counting cards. Then the REINFORCE method is used to try and find an optimal policy for each state.

To run the code simply import the .R files and use Rstudio or other IDE to run the main.R file. To see a more detailed explanation of the code structure see codestructure.pdf.
To see a more detailed explaination of the code and analysis of the results see BlackJack_Report.pdf.
