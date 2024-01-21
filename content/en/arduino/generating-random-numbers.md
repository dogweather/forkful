---
title:                "Generating random numbers"
date:                  2024-01-20T17:48:18.324542-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generating random numbers"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers is like tossing a virtual dice — you get a number that's unpredictable. Programmers use randomness to add variability in games, simulate data, or make decisions without a set pattern.

## How to:

Arduino makes it painless to whip up some randomness. Here's how you get a random integer between 1 and 6, like a die roll.

```Arduino
void setup() {
  Serial.begin(9600);
  randomSeed(analogRead(0));  // Seed the random number generator with an analog value
}

void loop() {
  int randomNumber = random(1, 7);  // Get a random number between 1 and 6
  Serial.println(randomNumber);     // Print it
  delay(1000);                      // Pause for a second
}
```

Sample output could read `5`, then `3`, then `6`, and so on every second, always a surprise.

## Deep Dive

Before the randomSeed function existed, Arduino randomness was as unpredictable as sunscreen in a raincoat. That changed with the notion of 'seeding'. Seeding gives the random number generator a starting point using either a fixed value or something more whimsical like noise from an unused analog pin.

The `random` function is versatile but not cryptographically secure. It's good for games or blinking lights, but don't use it for passwords or anything serious security-wise. If you thirst for better randomness, libraries like `TrueRandom` might quench it. They use noise and other techniques for more unpredictability.

Under the hood, Arduino uses a pseudo-random number generator. It's 'pseudo' because if you seed it with the same number, you'll get the same series of random numbers — handy for debugging or replicating results.

## See Also

- Arduino's Random Number Functions: https://www.arduino.cc/reference/en/language/functions/random-numbers/random/
- The Lowdown on True Randomness from `TrueRandom` Library: http://www.leonardomiliani.com/en/2013/a-true-random-generator-for-arduino/
- Entropy and Randomness in Computers: https://www.arduino.cc/en/Tutorial/Foundations/RandomNumber
- Seeding Random Numbers: https://www.arduino.cc/en/Tutorial/Foundations/RandomSeed