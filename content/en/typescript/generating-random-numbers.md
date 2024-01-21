---
title:                "Generating random numbers"
date:                  2024-01-20T17:50:16.637941-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generating random numbers"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generating random numbers means creating numbers that are unpredictable or seem to lack any discernible pattern. Programmers use randomness in simulations, games, security systems (ever heard of cryptography?), and anytime they need a sprinkle of unpredictability in their code stew.

## How to:
```TypeScript
function getRandomInt(min: number, max: number): number {
  // Ensure the range is valid and min is less than max
  if (min >= max) {
    throw new Error('The "min" must be less than "max"');
  }
  // Math.random() generates a float between 0 and 1
  // Multiplying by (max - min) stretches our range
  // Adding min offsets the range to ensure it starts at min
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Let's roll a six-sided dice
console.log(getRandomInt(1, 6));

// Need a number between 10 and 20?
console.log(getRandomInt(10, 20));
```

Sample Output:
```
4
17
```

## Deep Dive
Since the dawn of programming, random numbers have been the bread and butter for a variety of applications. However, computers are deterministic beasts - random doesn't come naturally to them. They mimic randomness through algorithms, which inherently can't be truly random; we call them pseudo-random.

Historically, the quest for randomness even affected hardware design, with gadgets dedicated to generating random noise. Nowadays, our pseudo-random number generators (PRNGs) are so good that for most applications, they're indistinguishable from true random.

Although `Math.random()` in JavaScript and TypeScript does the job for simple tasks, it isn't cryptographically secure. If you need cryptographic randomness in a TypeScript app, head over to the `crypto` module. This has a method called `getRandomValues()`, which is suitable for cryptographic use.

One last thing—avoid using random numbers as the sole source of security unless you know what you're doing. When it comes to security, proper implementation and using tried-and-tested libraries are key.

## See Also
* [Mozilla Developer Network - Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
* [Mozilla Developer Network - Crypto.getRandomValues()](https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues)
* [Node.js crypto module](https://nodejs.org/api/crypto.html#cryptorandombytesize-callback)