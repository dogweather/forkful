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

## Why
Random numbers are an essential component of many programming projects, from generating unique IDs to creating randomized game elements. In Arduino, the `random()` function allows for the generation of pseudo-random numbers that can add an element of unpredictability to your project. 

## How To
To generate random numbers in Arduino, you can use the `random()` function. Here is an example code snippet:

```arduino
int randomNum = random(0, 10); // generates a random number between 0 and 10
Serial.println(randomNum); // outputs the random number to the serial monitor
```

Running this code would output a different random number each time it is executed. You can also specify a range for the random numbers by changing the values in the `random()` function. 

```arduino
int randomNum = random(50, 100); // generates a random number between 50 and 100
Serial.println(randomNum); // outputs the random number to the serial monitor
```

You can also use the `randomSeed()` function to generate different sequences of random numbers. Here is an example:

```arduino
randomSeed(analogRead(A0)); // uses a changing voltage on analog pin 0 to generate a different sequence of random numbers
int randomNum = random(0, 10); // generates a random number between 0 and 10
Serial.println(randomNum); // outputs the random number to the serial monitor
```

Note that the `randomSeed()` function should only be called once in the setup function, and not in the loop function. 

## Deep Dive
The `random()` function in Arduino is actually a modified implementation of the C function `rand()`, which uses a mathematical formula to generate pseudo-random numbers. This means that while the numbers generated appear random, they are actually following a predictable pattern. Hence, the term "pseudo-random."

The algorithm used in `random()` is called a linear congruential generator (LCG). It works by multiplying a seed value by a large number, adding another value, and then taking the remainder of that result when divided by a third number. This remainder becomes the next seed value, and the process continues to generate a sequence of numbers. The `randomSeed()` function allows us to change the initial seed value to create a different sequence.

It's also important to note that while `random()` can generate a wide range of numbers, it is not truly random and can produce similar numbers in consecutive runs. To overcome this, you can use external factors like an analog input or the millis() function to change the seed value and create a different sequence of numbers. 

## See Also
For more information on the `random()` function and other Arduino programming techniques, check out these resources:

https://www.arduino.cc/reference/en/language/functions/random-numbers/random/

https://learn.adafruit.com/arduino-tips-tricks-and-techniques/random-numbers

https://www.arduino.cc/en/Reference/RandomSeed