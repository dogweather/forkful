---
title:    "Gleam recipe: Generating random numbers"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Why

Generating random numbers is an important aspect of programming and can have many applications. This can include anything from creating randomized game elements to providing secure encryption keys. No matter the reason, having the ability to generate random numbers can greatly enhance the functionality and versatility of your programs.

## How To

To generate random numbers in Gleam, we can use the `random` module. Let's take a look at some code examples to see how it's done:

```
Gleam> import gleam/random
Gleam> let number = random.int(1, 10)
Gleam> number
5
```

In this example, we first import the `random` module and then use the `int` function to generate a random integer between 1 and 10. The output, as shown, is the number 5.

We can also generate random floats using the `float` function, and even random strings with the `string` function. Let's see these in action:

```
Gleam> import gleam/random
Gleam> let float = random.float(0.0, 1.0)
Gleam> float
0.467
Gleam> let string = random.string(10)
Gleam> string
"kjg4p18f9a"
```

As you can see, the `float` function generates a random float between 0.0 and 1.0, while the `string` function creates a random string of 10 characters.

## Deep Dive

Behind the scenes, the `random` module uses a high-quality pseudorandom number generator to generate its random values. This means that the numbers are not truly random, but rather generated in a specific sequence. However, this sequence is so complex and unpredictable that it is considered to be near-random.

One important thing to keep in mind when using the `random` module is that the same sequence of numbers will be generated every time the code is executed. This can be useful for testing purposes, but in many cases, it is desirable to have a different sequence each time. To achieve this, we can use the `random.seed` function to provide a different starting point for the random sequence each time the code is run.

## See Also

* [Gleam documentation on the random module](https://gleam.run/modules/stdlib/random/)
* [Difference between pseudorandom and truly random numbers](https://www.geeksforgeeks.org/difference-between-pseudo-random-number-generator-and-truly-random-number-generators/)
* [Real-world applications of random number generation](https://corporatefinanceinstitute.com/resources/knowledge/other/random-number-generator/)