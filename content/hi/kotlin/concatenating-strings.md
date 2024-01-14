---
title:                "Kotlin: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Kyun
String ko concat karna programming mein behad hi aam aur zaroori hai. Jab hum alag alag strings ko ek saath jodte hain, to humare code mein flexibility, readability aur functionality badh jati hai.

## Kaise Karein
Concatenating strings Kotlin mein behad hi asaan hai. Hum simple "+" operators ka istemal karke strings ko jod sakte hain. Iske liye, hum ek "plus" function ka istemal karte hain jo do strings ko ek saath jodkar ek naya string create karta hai. Chaliye isko ek example ke through samajhte hain:

```Kotlin
val firstName = "Aryan"
val lastName = "Sharma"
val fullName = firstName + " " + lastName
println(fullName)

// Output: Aryan Sharma
```
Is example mein humne pehle do variables, "firstName" aur "lastName" mein alag alag strings assign kiya. Fir humne "fullName" variable mein "+" operator ka istemal karke dono strings ko jodkar ek naya string create kiya. Aur iski output ko print kiya.

## Deep Dive
Humne dekha ki '+' operator ka istemal karke hum bahut asaani se strings ko jod sakte hain. Lekin Kotlin mein hum "plus" function ke sath sath "plusAssign" bhi function ka istemal kar sakte hain jo humare variable ko update karke usmein concat kiya hua string rakhta hai. Is tarah se humare code mein less clutter aur better performance milti hai.

## Dekhein Bhi
* [Official Kotlin Documentation on strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
* [Tutorial on concatenating strings in Kotlin](https://www.geeksforgeeks.org/kotlin-string-concatenation/)
* [Medium article on string manipulation in Kotlin](https://medium.com/codingtown/kotlin-string-manipulation-ffc3a5a0b445)