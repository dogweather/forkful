---
title:                "Kotlin: स्ट्रिंग को माज़बूत करना"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Kyu

Kotlin mein string ka capitalization karna aam baat hai kyunki ye bahut saari situations mein kaam aata hai jaise user input ko uniform banane ya fir strings ko visually alag dikhane ke liye.

## Kaise Kare

Capitalization ek string ko modify karne ka ek tareeka hai. Kotlin mein iske liye ```capitalize()``` aur ```toUpperCase()``` functions hote hain. Neeche diye gaye code blocks mein inka istemal kaise kiya ja sakta hai:

```Kotlin
val str = "hello, world!"

println(str.capitalize()) // Output: Hello, world!
println(str.toUpperCase()) // Output: HELLO, WORLD!
```

## Gehraai mein Jaae

Jab hum string ko uppercase karte hain, toh yeh function sabhi characters ko uppercase mein convert kar deta hai. Lekin capitalize function mein pehla character uppercase ban jata hai aur baaki characters as-is rehte hain. Isliye agar hum chahe toh capitalize ke saath dusre functions jaise ```toLowerCase()``` ka bhi istemal kar sakte hain.

## Dekhein Bhi

- Kotlin String Functions: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/index.html
- Capitalization in Kotlin: https://www.javatpoint.com/kotlin-string-function
- String Manipulation in Hindi: https://www.geeksforgeeks.org/strings-in-kotlin/