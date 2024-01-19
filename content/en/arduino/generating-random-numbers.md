---
title:                "Generating random numbers"
html_title:           "Arduino recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Random number generation is the process of creating a sequence of numbers in which each number doesn’t have any connection to the ones before it or after it. It is enormously useful in programming for tasks like simulating unpredictable events, generating unique identifiers, or adding variety to repetitive tasks.

## How To:

To generate a random number in Arduino, use the `random()` function:

```Arduino
// Generates a random number between 0 and 100
long rndNumber = random(100);
```

Let's diversify things up by setting a range:

```Arduino
// Generates a random number between 50 and 100
long rndNumber = random(50, 100);
```

The output will be a random number between the specified range.

Also, it's generally a good idea to seed (`randomSeed(analogRead(0));`) the pseudo-random number generator to what analogRead(0) is (some sensor noise) to enhance randomness. You do that in your `setup()` block:

```Arduino
void setup() {
  randomSeed(analogRead(0));
}
```

## Deep Dive

Historically, Arduino's `random()` function wasn't truly random - it was pseudorandom, meaning the sequence of numbers it generated was deterministic and predictable. However, sufficiently for many purposes, and randomly seeding your generator with outside data like we did with `analogRead(0)` made it 'random enough' in practice.

If you need stronger randomness, consider hardware random number generators or cryptographic libraries that offer better entropy.

About implementation, the `random()` function uses a linear congruential generator as its default algorithm, with parameters provided by Park and Miller in a 1988 paper. It's quick but predictable which is why it's wise to mix in some truly random data with `randomSeed()`.

## See Also

Check out more about Arduino's `random()` function in the official documentation, [here](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/).

For a more advanced method of generating hardware random numbers, see [Entropy Library](https://github.com/pmjdebruijn/Arduino-Entropy-Library).

To learn about various random number algorithms, [this page](https://en.wikipedia.org/wiki/List_of_random_number_generators) covers them in detail.