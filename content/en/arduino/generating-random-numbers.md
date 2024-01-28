---
title:                "Generating random numbers"
date:                  2024-01-27T19:45:08.197145-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generating random numbers"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers in Arduino projects is about creating unpredictable numeric values that can enhance the functionality of applications ranging from games to security systems. Programmers use this feature to introduce variability and simulate real-world phenomena.

## How to:

Arduino provides two main functions for generating random numbers: `random()` and `randomSeed()`. The `random()` function generates pseudo-random numbers in a specified range, while `randomSeed()` initializes the pseudo-random number generator to create more unpredictable sequences.

```Arduino
void setup() {
  Serial.begin(9600);

  // Initialize random number generator with a unique seed
  randomSeed(analogRead(0));

  // Generate a random number between 0 and 99
  int randomNumber = random(100);
  Serial.println(randomNumber);

  // Generate a random number between 50 and 200
  int randomNumberInRange = random(50, 201);
  Serial.println(randomNumberInRange);
}

void loop() {
  // This is just an example; normally, you wouldn't continuously initialize the seed
}
```

Sample output might look unpredictable each time you reset your Arduino, due to the random seed initialization:
```
45
153
```
On subsequent resets, because of `randomSeed(analogRead(0))`, you'll likely see different numbers, showcasing the unpredictability of the sequence.

## Deep Dive

Arduino's random number generation is based on a linear congruential generator (LCG), a common algorithm for generating sequences of pseudo-random numbers. The use of `randomSeed()` with a unique value, such as the noise from an unconnected analog pin (`analogRead(0)`), is crucial for enhancing the pseudo-random sequence's unpredictability. However, it's important to understand that this method is not suitable for cryptographic purposes, as the sequence can eventually become predictable to an observer with sufficient data.

For applications requiring true randomness or higher levels of unpredictability, external hardware random number generators (RNGs) can be interfaced with Arduino. These devices generate random numbers derived from truly random physical phenomena, significantly enhancing security for encryption and other critical applications.

In summary, while Arduino's built-in functions offer convenient pseudo-random number generation suitable for a wide range of applications, alternative methods or devices may be preferable for projects requiring higher security or truly random values.

## See also

### Official Arduino Documentation
- [Arduino `random()` Function](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)

### Tutorials and Guides
- **Arduino Project Hub**: [How to Generate Random Numbers with Arduino](https://create.arduino.cc/projecthub/electropeak/how-to-generate-random-numbers-with-arduino-5198b9)
- **Last Minute Engineers**: [Understanding Arduino Random Numbers - Generating Random Numbers & Seeds](https://lastminuteengineers.com/arduino-random-numbers-seed-tutorial/)

### YouTube Videos
- **Paul McWhorter**: [Arduino Tutorial: Using millis() Instead of delay() - A Simple Example](https://www.youtube.com/watch?v=3VZgaJ5B2aU)
- **Robojax**: [Random number with Arduino](https://www.youtube.com/watch?v=CpX5K-7WiA8)
