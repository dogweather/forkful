---
title:                "Javascript: अनियमित संख्याएं उत्पन्न करना"
simple_title:         "अनियमित संख्याएं उत्पन्न करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Kyun

Random numbers ka upyog kaaran randomization aur unpredictability hai. Ye shayad ek game me score ka random selection ho ya fir security ke liye passwords generate karne me upyogi ho. Kisi bhi situation me, random numbers ka Feature Javascript me bahut hi upyogi hai.

# Kaise Kare

Javascript me Math object ke andar ek built-in function hai `Math.random()`. Is function ko use karke, hum random numbers generate kar sakte hai. Example ke liye, agar hume 1 se 10 tak ke numbers me se koi bhi ek random number generate karna hai, toh hum ye code use kar sakte hai:

```Javascript
var randomNumber = Math.floor(Math.random() * 10) + 1;
console.log(randomNumber);
```

Is code se, hum ek random number generate karenge jo 1 se lekar 10 tak ke beech me hoga. `Math.floor()` function hume number ko round karne me madad karta hai aur `Math.random()` hume 0 se 1 tak ke numbers me se koi bhi ek number generate karne me help karta hai. Iska output ke roop me, hum console me kuch aisa dekhege:

> 7

# Gehraai Se Jaane

Random number generation ek important topic hai programming me aur isse samajhna bahut hi important hai. Isliye, hum thode aur details me jaanege.

Math object ke andar, ek aur function hai `Math.floor()`. Is function ki help se, hum decimal values ko bhi round kar sakte hai aur pure integers generate kar sakte hai. Agar hum `Math.floor()` ke jagah `Math.ceil()` function ko use karte hai, toh hum decimal numbers ko round kar sakte hai.

Ek aur important topic hai seed values. Seed value ek starting point hoti hai random number generation ki process ke liye. Agar hum kisi particular seed value ko use karte hai, toh hum hamesha same random numbers generate kar sakte hai. Isse hume predictions aur debugging me madad milti hai.

# Dekhiye Bhi

- [MDN Web Docs- Math.random()](https://developer.mozilla.org/hi/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Tutorialspoint - Generating random numbers in Javascript](https://www.tutorialspoint.com/generating-random-number-between-1-and-10-in-javascript)