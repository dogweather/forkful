---
title:                "उपस्थापित सबस्ट्रिंग्स"
html_title:           "Bash: उपस्थापित सबस्ट्रिंग्स"
simple_title:         "उपस्थापित सबस्ट्रिंग्स"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Kyun
Kya tum kabhi sochte ho ki kisi bade string se chote chote hisse ko extract kaise kiya ja sakta hai? Yeh Bash programming language mein possible hai. Is article mein hum extract substring ki technique ke baare mein jaanenge.

## Kaise Karein
```Bash
# Is syntax se hum kisi string ke pehle se dusri position tak ke characters ko extract kar sakte hain.
${string:position}

# Is syntax se hum string ke ek se zyada positions se characters ko extract kar sakte hain.
${string:position:count}
```
Is code snippet mein, "string" string ka original string hai, "position" us position ko represent karta hai jahan se extract karna shuru karna hai aur "count" kitne characters extract karna hai. Yeh code snippet sample output ke saath diya gaya hai jis se aap asani se samajh sakte hain ki kaise substring extract kiya jata hai.

## Deep Dive
Bash mein substring extraction do tarah se kiya ja sakta hai - pehla, kisi position se ek se zyada characters extract karna aur doosra, kisi position se kisi dusri position tak ke characters ko extract karna. Hum in dono methods ko detail mein dekhenge.

Kisi position se ek se zyada characters extract karne ke liye, hum "substring expansion" syntax ka istemal karte hain, jisme `$` ka istemal kiya jaata hai. Saath hi, hum ise `{string:position:count}` mein wrap karte hain. Agar hum position se characters extract karna chahte hain, to position value 0 se shuru ho sakti hai.

Kisi position se kisi dusri position tak ke characters ko extract karne ke liye, hum "substring expansion" syntax mein dono positions ko alag alag dene ke saath "$" aur "{string:position1:position2}" mein wrap karte hain.

## Dekhiye Bhi
- [Bash Substring Extraction Examples](https://www.tecmint.com/extract-domain-from-url-in-linux/)
- [Understanding Bash String Manipulation](https://www.linuxjournal.com/content/understanding-bash-string-manipulation)