---
title:                "Kotlin: उप-स्ट्रिंग्स ताक निकालना"
simple_title:         "उप-स्ट्रिंग्स ताक निकालना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

# Kyun: Substrings ko nikalne me kyu laga rahe hai?

Substrings ko nikalna kisi code ki flexibility aur readability ko bhadhata hai. Iske alawa, isse hum apne code ko optimize aur efficient banate hai. Substrings ka use kai alag alag situation me kiya ja sakta hai, jaise ki data cleaning, string manipulation, ya algorithms ki implementation me.

## Kaise Kare: Substrings ko nikalna

Substrings ko Kotlin me nikalne ke liye, hum `substring()` function ka use karte hai. Ye function hume ek range ya starting index aur ending index provide karta hai, jiske beech ke characters ko hum nikal sakte hai. Neechey diye gaye example ko dekhkar iska istemaal aur samajh sakte hai:

```
Kotlin val str = "Hello World"
val subString = str.substring(0, 5)

println(subString) // Output: Hello
```

Is code me, humne `substring()` function ka use kiya hai `str` string ki starting index 0 aur ending index 5 tak. Iss tarah se humne `Hello` substring nikala hai. Is tarah se hum apne code me koi bhi range ya starting aur ending index ka combination use karke substring nikal sakte hai.

## Gehri Khudayi: Substrings ka anant gyan

String manipulation ke liye, hum substring ka use karte hai. Isse hum apne strings ko extract aur manipulate kar sakte hai. Substrings ko niyamit taur par istemaal karke, hum apne code ko efficient aur optimized bana sakte hai. Substrings ko use karke, hum alag alag patterns aur combinations bana sakte hai, jisse hum apne code ko aur bhi powerful bana sakte hai.

## Dekhe Bhi:

- [Substring function in Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html)
- [Different ways to extract substrings in Kotlin](https://www.baeldung.com/kotlin/substring)
- [Kotlin Strings Tutorial](https://www.geeksforgeeks.org/kotlin-standard-strings/)