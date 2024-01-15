---
title:                "स्ट्रिंग को निचले भाग में बदलना"
html_title:           "Go: स्ट्रिंग को निचले भाग में बदलना"
simple_title:         "स्ट्रिंग को निचले भाग में बदलना"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Kyu

Agar aap apne Go programming skills ko improve karna chahte hain ya fir abhi tak aapne string ko lower case mein convert karne ka tarika nahi sikha hai, toh yeh article aapke liye hai. Yahan hum jaanenge ki kyu kisi bhi programmer ko string ko lower case mein convert karna jaruri hai.

## Kaise Kare

Agar aap string ko lower case mein convert karna chahte hain, toh aapko `strings` package ka istemal karna hoga. Is package mein `ToLower()` function moujood hai jo kisi bhi string ko lower case mein convert karta hai. Chaliye isko ek code example ke through samajhte hain.

```
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "HELLO WORLD"
    lowerStr := strings.ToLower(str)
    fmt.Println(lowerStr)
}

```

Output:

```
hello world
```

Is code mein humne `ToLower()` function ka istemal kiya aur `HELLO WORLD` string ko lower case mein convert karke output ke roop mein print kiya. Is tarah se hum kisi bhi string ko lower case mein convert kar sakte hain.

## Gehri Jankari

Jab bhi ham kisi programming language mein code likhte hain, toh string ka use bahut common hota hai. Isliye string ko lower case mein convert karna bhi bahut zaruri ho jata hai. Yeh string comparison mein help karta hai kyun ki lower case mein likhi gayi string ko comparison karne mein programming language ko jyada samay nahi lagta. Isse code ki execution time bhi kam hoti hai.

## See Also

Ab aapko pata chal gaya hoga ki kyu kisi bhi programmer ko string ko lower case mein convert karna zaruri hai aur kaise hum is task ko asani se kar sakte hain. Agar aapko aur bhi Go programming ke bare mein jaankari chahte hain, toh neeche diye gaye links zarur check kare.

- [Go Language Documentation](https://golang.org/doc/)
- [Go Language Tour](https://tour.golang.org/welcome/)