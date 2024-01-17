---
title:                "रैंडम नंबर्स उत्पन्न करना"
html_title:           "TypeScript: रैंडम नंबर्स उत्पन्न करना"
simple_title:         "रैंडम नंबर्स उत्पन्न करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

Kya aur Kyun?: Random numbers generate karna kya hai aur programmer kyun ise karte hain?

Random numbers generate karna, kisi bhi computer program ke liye aise numbers ko create karna hai jo ki "random" hote hain. Iska matlab hai ki yeh numbers hamesha unpredictable hote hain, jisse programmers apne programs ko aur bhi interesting aur secure bana sake.

Kaise Karne ka Tarika: TypeScript ke andar random numbers generate karne ke liye, aapko sirf ek function ka use karna hoga - `Math.random()`. Is function ko use karne ke liye, aapko bas apne program ke andar ek `for` loop chalana hoga jo specific number of iterations ke liye chalega aur har iteration mein random numbers generate karega. Neeche diye gaye code block mein, hum ek program dekhenge jo 1 se lekar 100 tak ke random numbers print karega.

```TypeScript
for (let i = 1; i <= 100; i++) {
  console.log(Math.random());
}
```

Is program ke output kuch iss tarah hoga:

```
0.4639815641054306
0.15180590561028824
0.9423428730893207
0.2693024657194499
0.71460948261993
0.9361281372015226
0.4915975440183646
0.8025146052172781
0.8854412119908747
0.06328897579228288
...
```

Gehri Jhanjhavat: Random numbers generate karna ke peechey ek gehra sa background hai. Random numbers apne aap mein unpredictable hote hain, isiliye inhe cryptography aur security ke liye bhi istemal kiya jaata hai. Duniya mein kai algorithms aur techniques hain random numbers generate karne ke liye jinmein se kuch aapne aajtak suna hoga jaise Linear Congruential Generator aur Blum Blum Shub. TypeScript ke andar `Math.random()` function bhi kisi ek algorithm ka use karta hai jo ki C++ ka `rand` function se derived hai.

Humein aur jaanna hai toh aap in links ko check kar sakte hain:

- [Random number generation in TypeScript](https://www.typescriptlang.org/docs/handbook/faq.html)
- [C++ rand() function](https://www.cplusplus.com/reference/cstdlib/rand/)

As usual, humne abhi sirf ek chhuotey se overview diya hai random numbers generate karne ke baare mein aur ummeed karte hain ke aapke dimaag mein abhi aur clearer ho gaya ho. Keep coding and stay curious!