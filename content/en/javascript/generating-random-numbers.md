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

## What & Why?

Generating random numbers is a built-in feature in Javascript that allows programmers to generate pseudorandom numbers for various purposes. From creating random games and simulations to enhancing security with randomized keys, random numbers are essential in modern programming.

## How to:

To generate a random number in Javascript, we can use the ```Math.random()``` method. This method returns a number between 0 (inclusive) and 1 (exclusive). To get a random number within a specific range, we can use the formula ```Math.random() * (max - min) + min```. For example, if we want a random number between 1 and 10, we can use ```Math.random() * (10 - 1) + 1```, which will give us a value between 1 (inclusive) and 10 (exclusive).

```
// Generate a random number between 1 and 10
let randomNumber = Math.random() * (10 - 1) + 1;
console.log(randomNumber); // output: 5.743568029182

// Generate a random integer between 1 and 10
let randomInteger = Math.floor(Math.random() * (10 - 1 + 1) + 1);
console.log(randomInteger); // output: 7
```

## Deep Dive:

The ```Math.random()``` method has been a part of the Javascript language since its inception in 1995. It uses a mathematical algorithm to generate a sequence of pseudorandom numbers that appear to be random. However, this algorithm can still produce patterns and should not be used for critical security purposes. For more secure random numbers, specialized libraries and tools are recommended.

There are alternative methods to generate random numbers in Javascript, such as using the ```Date``` object or libraries like Lodash. These methods may provide different levels of randomness and can be used based on specific needs.

## See Also:

- [MDN Web Docs - Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [FreeCodeCamp - How to Generate Random Numbers in JavaScript](https://www.freecodecamp.org/news/how-to-generate-random-numbers-in-javascript/)
- [Lodash - Random](https://lodash.com/docs/4.17.15#random)