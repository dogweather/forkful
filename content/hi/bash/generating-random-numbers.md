---
title:                "यादृच्छिक संख्याएं उत्पन्न करना"
html_title:           "Bash: यादृच्छिक संख्याएं उत्पन्न करना"
simple_title:         "यादृच्छिक संख्याएं उत्पन्न करना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

Kya hai aur kyun kare?
Generating random numbers ek aisi coding technique hai jiske through hum ek arbitrary ya unpredictable sequence of numbers ko generate kar sakte hain. Programmers isko use karte hain for various purposes such as creating unique IDs, simulation, encryption, and so on. Iske alawa, isse security purposes ke liye bhi use kiya ja sakta hai.

Kaise kare?
Generating random numbers Bash mein kaafi asaan hai. Hamare paas ```$RANDOM``` built-in variable hai jo ek random integer value generate karta hai har bar jab hum isey call karte hain. Isi tarah hum ```/dev/urandom``` location ko bhi use kar sakte hain jaha se hum random bytes ko read kar sakte hain. Neeche hum ek simple example dekhte hain:

```Bash
# $RANDOM variable se random integer generate karein
echo $RANDOM
# /dev/urandom se random bytes read karein aur unko base64 format mein display karein
cat /dev/urandom | base64
```

Sample output:

```Bash
28764
XV9crzStv+Bp89YhH1dzZrCoQodP9ZNDAjhM98z0eGAoICLQANarLIx6LT+JyA==
```

Gehri Khurak:
Generating random numbers ko implement karne ke liye, bash shell ka use kiya ja sakta hai. Iske alawa, hum Python, Java, ya phir C++ jaise bhi programming languages mein bhi iska use kar sakte hain. In languages mein random number generators ya phir ```crypto/rand``` module available hota hai jo ki secure random numbers generate karta hai.

Itna hi nahi, hum ek specific range ke numbers bhi generate kar sakte hain using some simple arithmetic. Jaise ki, agar hum chahte hain ki random numbers sirf 1 se lekar 10 tak hi generate ho, toh hum ```expr``` command ka use kar sakte hain:

```Bash
# Random number generate karein from 1 to 10
expr $RANDOM % 10 + 1
```

Link:
Generating random numbers ke liye aap https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html ya phir https://linuxhint.com/random_number_generator_bash/ jaise sources ko refer kar sakte hain.

Ab aap ko Bash mein random numbers generate karne ka process pata chal gaya hai. Toh aap paas wale security codes ke ek naye set ya phir apne kisi project mein use karne ke liye ab ready hain! Keep coding and happy generating!