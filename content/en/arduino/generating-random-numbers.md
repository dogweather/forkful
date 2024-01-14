---
title:                "Arduino recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why
Random numbers are essential in various applications such as statistical analysis, cryptography, and simulations. As an Arduino enthusiast, understanding how to generate random numbers can enhance your projects and make them more unpredictable and secure.

## How To
Generating random numbers in Arduino is relatively straightforward. Follow these steps to get started:

1. Import the `ArduinoRandom.h` library by adding `#include <ArduinoRandom.h>` at the top of your sketch.

2. Declare a `RandomSeed` by calling the `arduinorandom_seed()` function.

   ```arduino
   ArduinoRandomSeed();
   ```

3. Generate a random number within a specific range by using the `arduinorandom()` function. For example, to generate a random number between 0 and 10, use the following code:

   ```arduino
   int randomNumber = arduinorandom(0, 10);
   ```

4. Print the generated random number to the serial monitor by using the `Serial.println()` function:

   ```arduino
   Serial.println(randomNumber);
   ```

5. Upload the code to your Arduino board and open the serial monitor to see the output.

Sample output:

```
4
```

## Deep Dive
Behind the scenes, the `arduinorandom()` function uses a **pseudo-random number generator** (PRNG) algorithm called the *Linear Congruential Generator* (LCG). This algorithm involves multiplying the previous random number by a constant, adding another constant, and taking the modulo of the result to get the next random number.

To generate a truly random number, use an external hardware component such as a noise source or a geiger counter. This method is known as **true random number generation** (TRNG) and provides a higher level of unpredictability compared to PRNG.

It is important to note that while TRNG provides a better level of randomness, it can be slower and more costly to implement compared to PRNG. Therefore, it is essential to consider the specific needs of your project before deciding which method to use.

## See Also
- [Arduino Official Website](https://www.arduino.cc/)
- [ArduinoRandom Library GitHub Repository](https://github.com/chkarloz/ArduinoRandom)
- [Understanding Random Number Generators](https://www.random.org/randomness/)