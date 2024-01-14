---
title:                "Bash: पैटर्न से मेल खाने वाले अक्षरों को हटाना"
simple_title:         "पैटर्न से मेल खाने वाले अक्षरों को हटाना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Kyun
Aksar hume coding karte hue kuch aise situation aate hai jahan hume kisi specific pattern ke characters ko delete karna padta hai. Isme kisi specific word ya phrase ki jagah space ya kuch aur character ko hata dene ka kaam hota hai. Iss prakar ke kaam ko karne ke liye hum ek special tool ya technique ka istemaal karte hai jo hume code ko efficient banane aur errors ko kam karne me madad karta hai. Iska naam hai "character matching pattern delete".

## Kaise Kare
Iss coding task ko karne ke liye hum "Bash" programming language ka istemaal karte hai. Yeh ek command-line interface based language hai jo UNIX system par run karta hai. Iska simple syntax aur easy to use commands aapko character matching pattern ko delete karne me madad karte hai. Neeche diye gaye Bash code blocks me hum aapko iss task ka kuch examples dikhayenge:

```
# Example 1: Delete all spaces in a string
my_string="Hello World"
echo "${my_string// }"
# Output: HelloWorld

# Example 2: Delete all vowels in a string
my_string="Let's code in Bash"
echo "${my_string//[aeiouAEIOU]}"
# Output: Lt's cd n Bsh
```

Jaisa ki aap dekh sakte hai, humne apne string me se specific characters ko hataya hai bass ek simple command use karke. Isi tarah, aap apne coding tasks me "character matching pattern delete" ka istemaal kar sakte hai aur apne code ko aur efficient bana sakte hai.

## Deep Dive
Iss technique ko use karne ke liye hume curly brackets ke baad slash (/) lagana hota hai. Iske baad hum apne desired characters ya pattern ko likhte hai aur uske baad hum replacement characters ya patterns likhte hai. Agar hum replacement characters ya patterns nahi likhte, tab humare original string se specified characters delete ho jate hai. Agar aap chahe toh aap multiple characters ya patterns ko bhi delete kar sakte hai, jaise humne apne examples me kiya hai.

Iss technique me "globbing" ka use hota hai jiska kaam hai characters ya patterns ko match karna. Isme aap wildcard characters (*, ?, [ ]) ka bhi istemaal kar sakte hai.

## Dekhein Bhi
- [Bash scripting tutorial in Hindi](https://www.javatpoint.com/bash-scripting-tutorial)
- [Bash string manipulation techniques in Hindi](https://www.linuxteach.ml/2020/07/bash-string-manipulation-in-hindi.html)
- [Bash pattern matching and substitutions](https://www.tutorialspoint.com/unix/unix-regular-expressions.htm)

Asha hai ki aapko iss article se Bash programming language ke iss advanced technique ke bare me thoda aur pata chala hoga aur ab aap iss technique ka istemaal apne coding tasks me kar sakte hai. Yeh ek powerful tool hai aur aapke coding experience ko kafi improve karega. Dhanyawaad!