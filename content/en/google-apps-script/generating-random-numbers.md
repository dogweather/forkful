---
title:                "Generating random numbers"
date:                  2024-02-01T13:42:08.620841-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generating random numbers"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/google-apps-script/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generating random numbers in Google Apps Script is all about creating unpredictable values. Programmers do this for a variety of reasons, ranging from making decisions to simulating real-world scenarios in apps and games.

## How to:
Google Apps Script, based on JavaScript, makes it pretty straightforward to generate random numbers. Here's how you do it:

To get a random number between 0 (inclusive) and 1 (exclusive), you simply use the `Math.random()` function. Check this out:

```Javascript
function getRandomNumber() {
  var randomNumber = Math.random();
  Logger.log(randomNumber);
}
```

But what if you want a random integer within a specific range? Say, between 10 and 20? Here's a neat way to achieve that:

```Javascript
function getRandomInteger(min, max) {
  var range = max - min + 1; // +1 to make max inclusive
  var randomNumber = Math.floor(Math.random() * range) + min;
  Logger.log(randomNumber);
  return randomNumber;
}
```

Sample output for `getRandomInteger(10, 20)`: could be any integer between 10 and 20, like 15 or 18.

And, if you're into generating random unique IDs or strings, here's a quick method for that:

```Javascript
function generateRandomString(length) {
  var characters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
  var randomString = '';
  for (var i = 0; i < length; i++) {
    randomString += characters.charAt(Math.floor(Math.random() * characters.length));
  }
  Logger.log(randomString);
  return randomString;
}
```

This awesome function lets you specify the length of the string, and it outputs something like "Jk8n29Dn".

## Deep Dive
The under-the-hood mechanism of `Math.random()` in JavaScript, and hence Google Apps Script, isn't truly random. It's what we call "pseudo-random", based on algorithms that generate number sequences that appear random. 

For most applications - games, simple simulations, or everyday scripts - the pseudo-randomness of `Math.random()` is perfectly sufficient. However, when it comes down to cryptographic applications or scenarios where unpredictability is critical, this method falls short. For such cases, using a more robust randomness source, such as crypto APIs available in other environments, is recommended.

Interestingly, the algorithm behind `Math.random()` isn't defined by JavaScript's specification, ECMAScript, which means different JavaScript engines might implement it differently. Hence, the sequence and quality of randomness might vary across platforms. This usually isn't a concern when working within the Google Apps Script environment, but it's good to keep in mind if your script depends heavily on the quality of randomness.
