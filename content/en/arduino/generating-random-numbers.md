---
title:                "Arduino recipe: Generating random numbers"
programming_language: "Arduino"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Generating random numbers can be a valuable tool in Arduino programming. It allows for creating unpredictable and varied inputs for your projects, making them more engaging and versatile. Additionally, it can add an element of surprise to your code, making it more interesting for both you and your audience.

## How To

To generate random numbers in Arduino, we can use the built-in function `random()`, which takes in two arguments for the range of numbers you want to generate. For example, `random(0, 10)` would generate a random number between 0 and 9. Let's see an example:

```Arduino
int randomNumber = random(0, 10); //generate a random number between 0 and 9
Serial.println(randomNumber); //print the generated number to the serial monitor
```

The output would look something like this:

```
3 //randomly generated number
```

You can also use the `random()` function to generate random numbers for specific data types, such as floats and characters. For floats, you can use `random(0.0, 10.0)` to generate a random float between 0.0 and 9.9. For characters, you can use `random('a', 'z')` to generate a random lowercase character between a and y. Make sure to check the documentation for the full list of supported data types and their corresponding arguments.

## Deep Dive

The `random()` function in Arduino uses a pseudo-random number generator algorithm called the "Mersenne Twister". This algorithm generates numbers that appear to be random, but are actually deterministic. This means that the same sequence of numbers will be generated every time you run your code. To prevent this, we can use the `randomSeed()` function to set a "seed" value, making the random numbers more unpredictable. The `randomSeed()` function takes in a number as its argument, and it is recommended to use an analog input pin for the seed value to introduce some randomness to the sequence. Additionally, you can also use `randomSeed(analogRead(A0))` to use the analog value from pin A0 as the seed value.

## See Also

For more information on generating random numbers in Arduino, check out the following resources:

- [Arduino Reference - random()](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Arduino Project Hub - How to Generate Random Numbers in Arduino](https://create.arduino.cc/projecthub/ruchir1674/generating-random-numbers-in-arduino-890252)
- [Arduino Forum - How to Use randomSeed()?](https://forum.arduino.cc/index.php?topic=38443.0)