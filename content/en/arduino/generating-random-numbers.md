---
title:    "Arduino recipe: Generating random numbers"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Why

Have you ever wanted to add an element of randomness to your Arduino projects? Maybe you're making a game or a simulation and you want to generate random numbers to keep things interesting. Or perhaps you just want to add some unpredictability to your LED light patterns. Whatever your reason may be, generating random numbers in an Arduino program can open up a whole new world of possibilities.

## How To

Generating random numbers in an Arduino program is actually quite simple. The first thing you need to do is declare a variable to store the random number. This can be an integer or float, depending on your needs. Then, you can use the `random()` function to generate a random number within a specified range.

For example, if you want to generate a random number between 0 and 100, your code would look like this:

```Arduino
int randomNumber = random(0, 100); // generates a random number between 0 and 99
```

You can also use the `random()` function to generate a random number within a specific data type range. For example, if you want to generate a random ASCII character, you can use the `random()` function with the `char()` function, like this:

```Arduino
char randomChar = char(random(33, 126)); // generates a random ASCII character between ! and ~
```

You can also generate a series of random numbers by using a for loop. This is useful if you need to generate a larger set of random numbers for a game or simulation. Here's an example:

```Arduino
for(int i = 0; i < 10; i++) { // will generate 10 random numbers
  int randomNumber = random(1, 10); // generates a random number between 1 and 9
  Serial.println(randomNumber); // prints the random number to the Serial Monitor
}
```

The output of this code would be something like this:

```
4
9
2
6
1
7
8
3
5
10
```

## Deep Dive

So how does the `random()` function actually work? The Arduino uses a algorithm called the linear congruential generator to generate random numbers. This algorithm uses a simple mathematical formula to generate a series of seemingly random numbers. The formula is:

```
Xn+1 = (a * Xn + c) % m
```

Where `Xn+1` is the next random number, `a` is a multiplier, `c` is an increment, and `m` is a modulus. The values of these parameters are determined by the Arduino library, but you can also customize them if you'd like. However, it's important to note that the linear congruential generator is not a truly random algorithm and the numbers it generates may not be completely unpredictable.

## See Also

For more information on generating random numbers in Arduino, check out the following resources:

- [Arduino random() function documentation](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Tutorial on random number generation in Arduino](https://www.arduino.cc/en/tutorial/random)
- [Random number generator library for Arduino](https://github.com/mathur1712/Random_arduino)

Now that you know how to generate random numbers in Arduino, go ahead and add some unpredictability to your projects!