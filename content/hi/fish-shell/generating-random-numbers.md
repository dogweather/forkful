---
title:                "कंप्यूटर प्रोग्रामिंग पर 'रैंडम नंबर्स उत्पन्न करना'"
html_title:           "Fish Shell: कंप्यूटर प्रोग्रामिंग पर 'रैंडम नंबर्स उत्पन्न करना'"
simple_title:         "कंप्यूटर प्रोग्रामिंग पर 'रैंडम नंबर्स उत्पन्न करना'"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Kyon

Iss article mein hum jaanenge ki kyon kisi ko random numbers generate karne mein interest hota hai. Random numbers generation ki zaroorat kisi bhi programming language mein hoti hai, jaha par hume unpredictable input ki zaroorat hoti hai. Random numbers ka use cryptography, simulation, game development, aur bahut saari aur jaagaon mein kiya jata hai.

# Kaise Karein

Fish Shell mein random numbers generate karne ke liye bahut simple aur easy to use ek command hai - `random`. Is command ka use karke hum anek tarah ke random numbers generate kar sakte hain. Chaliye kuch examples dekhate hain:

```Fish Shell
random  # ek random floating point number banega
random 100  # ek random integer number banega 0 se 100 tak
random -r 10 20  # ek random floating point number banega 10 aur 20 ke beech
```

Agar hume specific range ke numbers generate karne hain, to hum `seq` command ka bhi use kar sakte hain:

```Fish Shell
seq (random 1 5)  # yeh command randomly 1 aur 5 ke beech koi ek integer number banayegi, fir us number tak ka sequence create karegi
```

Fish Shell mein hum `random` command ke saath kuch options bhi specify kar sakte hain, jaise `-r` jo ki humare input ki range ko specify karta hai aur `-l` jo ki humare output numbers ko left aligned kar deta hai. Is tarah hum apne use case ke hisaab se random numbers generate kar sakte hain.

# Deep Dive

Random numbers generate karne ke bahut sare algorithms hain, aur har algorithm ki apni strengths and weaknesses hoti hain. Fish Shell mein `random` command ka use karne se hum Mersenne Twister algorithm ka use karte hain. Ye algorithm ek powerful aur fast random number generator hai. Magar agar hum randomly generated numbers ko security sensitive jagah use karte hain, jaise cryptography, to hume ise secure algorithm ki jagah se badal kar use karna chahiye. Isliye, apne use case ke according hume sahi algorithm ka use karna zaroori hai.

# See Also

- [Fish Shell Documentation](https://fishshell.com/docs/current/)
- [Mersenne Twister algorithm](https://en.wikipedia.org/wiki/Mersenne_Twister)
- [Cryptography](https://en.wikipedia.org/wiki/Cryptography)