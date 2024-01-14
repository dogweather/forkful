---
title:    "Arduino recipe: Generating random numbers"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why 
Generating random numbers is a crucial aspect of programming and can be useful in a variety of projects. Whether you need random inputs for a game or want to produce unique IDs, knowing how to generate random numbers using Arduino can come in handy.

## How To
Using Arduino, generating random numbers is a relatively simple process. First, we need to include the `random()` function from the Arduino library. This function generates a random number between 0 and 4294967295 (the maximum value that can be represented with a 32-bit unsigned integer).

```Arduino
#include <Arduino.h>

void setup() {
  // initialize serial communication
  Serial.begin(9600);
  // generate random number
  unsigned long randomNum = random();
  // print the result
  Serial.print("The random number is: ");
  Serial.println(randomNum);
}

void loop() {
  // do nothing
}
```
Once we upload this code to our Arduino board, we should see a different random number printed in the Serial Monitor each time we run the code.

We can also specify a range in which we want to generate random numbers. For example, if we want a random number between 1 and 10, we can use the `random(min, max)` function.

```Arduino
// generate random number between 1 and 10
unsigned int randomNum = random(1, 11);
```

In addition to integers, the `random()` function can also generate random floating-point numbers. To do this, we use the `random(min, max)` function and specify the data type as `float`.

```Arduino
// generate random floating-point number between 0.0 and 1.0
float randomFloat = random(0.0, 1.0);
```

## Deep Dive
The `random()` function uses a mathematical algorithm called the Linear Congruential Generator (LCG) to create the randomness. This algorithm works by taking a starting value (known as the seed) and applying a series of mathematical operations to it. The seed value is typically based on the current time, which ensures that the sequence of numbers produced is different each time the code runs.

While the `random()` function is sufficient for most applications, for more complex and secure random number generation, Arduino also provides the `randomSeed()` function. This function allows us to set a custom seed value, which can be useful in cryptography applications where a non-predictable seed is necessary.

## See Also 
- [Arduino Reference - Random](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Arduino Random Numbers: What Are They and How Do They Work?](https://www.makerspaces.com/arduino-random-numbers/)
- [Generating Random Numbers with Arduino](https://allaboutarduino.com/generate-random-number-arduino/)