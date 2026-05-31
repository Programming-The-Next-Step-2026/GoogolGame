# GoogolGame

<!-- badges: start -->
[![R-CMD-check](https://github.com/Programming-The-Next-Step-2026/GoogolGame/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Programming-The-Next-Step-2026/GoogolGame/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Do You Know This Problem? 

You just finished your workout and it's time to shower. But first — that email you've been putting off all day. Then the dishes, and the floor isn't going to vacuum itself. After all that housework you deserve a little reward, so you sit down to finish your favourite series. By now the sweat has long dried, so it barely matters anyway. And it's late and your roommates are asleep, so it would be rude to shower. And you were planning to work out again tomorrow anyways — so what's the point of showering at all? You could go on like this forever.

The question is: **when is the right moment**? To stop and say: now I decide to do it?

To take the shower, to take out the trash, or to pick the card with the highest number…

The card with the highest number, you ask? The idea comes from Martin Gardner who first described a gambling game called Googol.

## What is the Googol Game?

The Googol Game demonstrates optimal stopping theory. The rules are simple: Player 1 writes any numbers they like on a set of cards, then turns them face down and shuffles them. Player 2 turns over one card at a time and must decide when to stop — betting that the current card is the highest. Crucially, you cannot go back. If you pass a card, it's gone.

Is there an optimal point to stop?

Explore this question with the Googol Game app!

## Installation

```r
# install.packages("devtools")
devtools::install_github("Programming-The-Next-Step-2026/GoogolGame")
```

## Usage

```r
GoogolGame::play_googol()
```

## Features

- **Random mode**: the app generates a sequence of numbers across a wide range of scales — from small integers up to Googol-sized values (e.g. "3 Million", "7 Googol").
- **Manual mode**: enter your own numbers one at a time using a base value and a multiplier (None, Million, Billion, Trillion, Googol).
- Players can reveal cards one at a time and decide to stop if they think they found the highest number.
- Win and loss messages are displayed, with an option to play again.
