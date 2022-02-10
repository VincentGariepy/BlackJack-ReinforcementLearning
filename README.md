# BlackJack-ReinforcementLearning
Here are two approaches to trying to solve the optimal policy of a Black Jack game.  The Monte Carlo method simulates games of BlackJack with 6 decks and counts the card using the standard method, then tries to see if there is a relationship between the count of cards and the outcome of a game and the optimal policy.  The REINFORCE method, approoches the game with a random game, meaning there are infinite cards and any can be drawn at every step, so no counting cards. The uses the REINFORCE method to try and find an optimal policy for each state.