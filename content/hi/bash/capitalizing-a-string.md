---
title:                "एक स्ट्रिंग को कैपिटलाइज़ करना"
html_title:           "Bash: एक स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "एक स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Kyun
Agar aapko ek string mai woh alphabets ko uppercase (bada) karna hai jo ki pahle se nahi the, to yeh article aapke liye hai.

## Kaise Kare
**NOTE**: Yaha ```Bash ... ``` code blocks mai diye gaye examples mai, input ko `$str` variable mai store kiya gaya hai.

1. Sabse pahle, `tr` command ka use karke `tr [:lower:] [:upper:]` kar ke ek naya string create kare.

```Bash
$str="hello, world!"
echo $str | tr [:lower:] [:upper:]
```

**Output**: `HELLO, WORLD!`

2. Agar aapko sirf kuch specific alphabets ko bada karna hai, to unki jagah par `tr -d` use karke unko hata sakte hai aur fir `tr [:lower:] [:upper:]` se uppercase kar sakte hai.

```Bash
$str="hello, world!"
echo $str | tr -d '!' | tr [:lower:] [:upper:]
```

**Output**: `HELLO WORLD`

## Deep Dive
- `tr` command ki madad se hum ek string ko dusre string mai convert kar sakte hai.
- `[:lower:]` aur `[:upper:]` characters set ko use karne se alphabets ko uppercase mai convert kiya ja sakta hai.
- Agar aap sirf ek string ka specific part uppercase karna chahte hai, to `tr -d` command ka use kar sakte hai jisse us specific part ko string se remove kar diya jayega.

## Dekhe Bhiye
Aur bhi helpful articles padhne ke liye, niche diye gaye links par click kare:

- [Bash Basics in Hindi](https://idownvotedbecau.se/bash-basics-in-hindi)
- [Bash Guide for Beginners](https://www.tldp.org/LDP/Bash-Beginners-Guide/html)