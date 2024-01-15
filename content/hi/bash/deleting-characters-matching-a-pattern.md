---
title:                "पैटर्न मैचिंग करें उतारना"
html_title:           "Bash: पैटर्न मैचिंग करें उतारना"
simple_title:         "पैटर्न मैचिंग करें उतारना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Kyun
Kisi bhi programming language mein, humein kabhi kabhi pattern matching ka use karna padta hai. Pattern matching se humein specific characters, words ya phrases ko identify aur manipulate karne mein madad milti hai. Bash mein, hum kisi bhi pattern se match karne wale characters ko delete kar sakte hain, jo humari coding process ko aur efficient aur error-free bana deta hai.

## Kaise Karein
Kabhi kabhi humare Bash scripts mein kuch galat characters ki wajah se errors ya unwanted output aata hai. Is problem ko solve karne ke liye hum `sed` command ka use kar sakte hain. Ye command humein ek specified pattern ke match karne wale sabhi characters ko delete karne mein help karti hai. Neeche diye gaye code blocks mein hum dekhenge ki kaise hum `sed` command ka use karke kisi bhi pattern se match karne wale characters ko delete kar sakte hain.

```Bash
# Sample input
echo "Hello Worlld" 

# Output without using sed
Hello Worlld 

# Output using sed
echo "Hello Worlld" | sed 's/l//g'
He Wodd
```

Jaise hum dekh sakte hain, `sed 's/l//g'` command se humne "l" character ko delete kar diya hai. Is tarah hum kisi bhi pattern se match karne wale characters ko delete kar sakte hain. Agar humein sirf ek particular character ko delete karna hai, to hum `%` ka use karke bhi kar sakte hain. Chaliye deep dive section mein hum aur details ke baare mein jaante hain.

## Khulasa
Bash mein `sed` command ka use text manipulation ke liye bahut common hai. Is command mein, hum `-e` option ka use karke multiple commands ko bhi ek sath run kar sakte hain. Iske alawa, hum `sed` command se specific characters ko delete karne ke liye `[range]d` ya `%d` ka use kar sakte hain, jaha `[range]` humein specific line ya character range provide karta hai. Is command mein, humein `s/search/replace/` ka use pattern ko replace karne ke liye bhi kar sakte hain.

## See Also
- [How To Use the sed Command](https://www.digitalocean.com/community/tutorials/how-to-use-the-sed-command)
- [Bash Scripting Tutorial for Beginners](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)