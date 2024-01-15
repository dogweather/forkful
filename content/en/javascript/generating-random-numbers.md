---
title:                "Generating random numbers"
html_title:           "Javascript recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to generate a random number in your Javascript code? Maybe you're creating a game with randomized events or generating unique IDs for your database. Whatever the reason may be, generating random numbers can be a useful tool in your programming arsenal.

## How To

### Using Math.random()

To generate a random number in Javascript, we can use the built-in `Math.random()` method. This method returns a random number between 0 (inclusive) and 1 (exclusive). To use it, simply assign the returned value to a variable:

```Javascript
let randomNumber = Math.random();
console.log(randomNumber); // output: 0.5789633412465187
```

To generate a random number within a specific range, we can use a little bit of math. For example, if we wanted to generate a random number between 1 and 10, we can multiply the returned value by the range and add the minimum value:

```Javascript
let min = 1;
let max = 10;
let randomNumber = Math.random() * (max - min) + min;
console.log(randomNumber); // output: 8.037421541282188
```

### Using the Random Number API

If you need a more sophisticated way of generating random numbers, you can also use an API like [Random.org](https://www.random.org/) or [Random Number API](https://randomnumberapi.com/) to generate random numbers based on atmospheric noise or quantum randomness. These APIs typically have more options for customization and can be useful for security purposes.

## Deep Dive

When it comes to generating random numbers, it's important to note that they are not truly random. Computers work based on algorithms, so any random number generated will be pseudo-random. This means that the numbers appear to be random, but they follow a specific pattern that can be predicted given the right information.

To improve the randomness of our generated numbers, we can use a technique called seeding. This involves providing a specific input to the random number generator rather than relying on its default seed, which is often based on the current time.

Additionally, it's important to use a reliable source for generating random numbers, especially for sensitive applications like cryptography. As mentioned earlier, APIs like Random.org and Random Number API can be good options for this.

## See Also

- [Math.random() documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Random.org](https://www.random.org/)
- [Random Number API](https://randomnumberapi.com/)