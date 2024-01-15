---
title:                "स्ट्रिंग की लंबाई का पता लगाना"
html_title:           "Elm: स्ट्रिंग की लंबाई का पता लगाना"
simple_title:         "स्ट्रिंग की लंबाई का पता लगाना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Kyon

Kya kabhi aapne socha hai ki string ke length ko find karna kyu zaruri hai? Ye bahut hi common si problem hai jo har programmer ke samne aati hai. Kisi bhi programming language mein string ko manipulate karne ke liye, uski length ko find karna zaruri hai. Isse hume pata chalta hai ki string mein kitne characters hai aur hum usse sahi tarike se handle kar sakte hain. Isliye, string ke length ko find karna ek bahut zaruri skill hai har programmer ke liye.

## Kaise

Agar aapne kabhi Elm programming language se kaam kiya hai, to aapko pata hoga ki isme strings ko represent karne ke liye "String" data type hota hai. Aur is data type ka ek function hai "length" jo string ki length ko find karne mein help karta hai.

To chaliye dekhte hain kaise hum is function ka use karke string ki length ko find kar sakte hain. Neeche diye gaye code block mein humne ek sample string "Namaste" declare kiya hai aur uski length ko find karne ke liye "length" function ka use kiya hai. Iske baad, humne "Console.log" function ka use karke output ko console pe print kiya hai.

```Elm
str = "Namaste"
length(str)
```

Output:
```
7
```

Jaise aap dekh sakte hain, "length" function ne hume string "Namaste" ki length, yani 7 return kiya hai. Isi tarah aap kisi bhi string ki length ko find kar sakte hain is simple se function ka use karke.

## Deep Dive

Agar hum iss problem ko deep dive karein to hume pata chalta hai ki strings ko manipulate karne ke liye hume unki length ko find karna kitna zaruri hai. Isse hume pata chalta hai ki string mein kitne characters hai aur hum usse sahi tarike se access aur handle kar sakte hain.

Elm programming language mein, strings immutable (unchangeable) hote hain. Yani, ek baar string create ho jane ke baad, hum usme koi changes nahi kar sakte hain. Isliye, agar hume string ko modify karna hai, hume uski length ko find karke, usme se kuch characters remove ya add karne honge aur fir uss naye string ko create karna hoga.

Isi liye, string ki length ko find karna ek bahut hi zaruri task hai jo har programmer ko perfect tarike se karna chahiye. Kyunki iske bina hum string ko manipulate nahi kar sakte hain aur apne desired output ko achieve nahi kar sakte.

## Dekhiye Bhi

- [Official documentation for Elm string functions](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Tutorial on working with strings in Elm](https://elmprogramming.com/strings.html)