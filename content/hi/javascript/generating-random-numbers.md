---
title:                "हासिल करें सांद्रता संख्याएं"
html_title:           "Javascript: हासिल करें सांद्रता संख्याएं"
simple_title:         "हासिल करें सांद्रता संख्याएं"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Random numbers are numbers that are generated using a specific algorithm in an unpredictable manner. Programmers use them in various applications such as games, simulations, and cryptography. 

## How to:
```Javascript
//To generate a random number between 0 and 1 (excluding 1), use the Math.random() function
let randNum = Math.random();
console.log(randNum); //0.543013952370924

//To generate a random number between two specified values (included), use the Math.random() function along with some simple calculations
function getRandom(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}
let randNum = getRandom(1, 10);
console.log(randNum); //6
```

## Deep Dive:
Generating random numbers has been a common problem in computer science. It was first addressed by John von Neumann in the 1940s when he devised a way to generate random bits using a combination of physical processes and mathematical algorithms. Since then, several other methods have been developed such as linear congruential generators, middle-square method, and Mersenne Twister. However, these methods can sometimes result in patterns and should not be used for cryptographic purposes. A more secure option is to use a true random number generator which utilizes unpredictable physical processes like atmospheric noise or radioactive decay.

## See Also:
- [Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Random Numbers and JavaScript: What You Need to Know](https://www.freecodecamp.org/news/random-numbers-and-javascript-what-you-need-to-know-680d524fccca/)
- [True and Pseudo Random Numbers](https://www.techopedia.com/definition/26811/true-random-number)