---
title:                "डीबग आउटपुट प्रिंट करना"
html_title:           "Kotlin: डीबग आउटपुट प्रिंट करना"
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Kyun

Kya aapne kabhi apne code mein bug ko dhoondhne ke liye debug output ka upyog kiya hai? Agar haan, to aap jaante honge ki ye koi aasaan kaam nahi hai. Debug output ka upyog karne se aap apne code ki durusti ko jaan sakte hain aur sahi prakaar se sahi jagah par kaam kar raha hai ya nahi. Isliye, debug output ko print karne ka ek accha tarika hai apne code ko improve karne ka.

## Kaise

Debug output ko print karne ke liye, aap ```print()``` ya ```println()``` ka upyog kar sakte hain Kotlin mein. Ye strings, integers, ya booleans jaise kisi bhi data type ki values ko print karte hain. Example ke liye, yahaan ek simple ```print()``` ka code hai:

```Kotlin
fun main(args: Array<String>) {
    val name = "John"
    val age = 25
    
    print("Hello, my name is $name and I am $age years old.")
}
```

Is code mein, humne ```name``` aur ```age``` ki values ko print kiya hai. Is tarah se, aap apne code mein kisi bhi variable ko debug output ke liye print kar sakte hain.

## Deep Dive

Debug output ka upyog aapke code ko measure karne ka accha tarika hai. Isse aapko pata chalta hai ki aapka code kis hisse mein slow hai ya kis jagah par error aaya hai. Aap apne code mein variables ko print karke unhe monitor kar sakte hain aur sahi jagah par sahi values aa rahe hain ya nahi.

Ek aur acchi baat ye hai ki debug output aapke code ko document bhi banata hai. Agar aap ya aapke team ke members baad mein aapke code ko dekhein to debug output se pata chalta hai ki code kaise kaam karta hai aur usme kya values use ho rahi hain.

## Dekhein Bhi

- https://kotlinlang.org/docs/tutorials/kotlin-for-py/print.html
- https://www.geeksforgeeks.org/debug-output-kotlin/
- https://www.raywenderlich.com/1531427-debugging-kotlin-apps-in-android-studio
- https://www.youtube.com/watch?v=fzjbhgxUYlM

Padhne ke liye dhanyavaad!

## Dekhein bhi

- https://kotlinlang.org/
- https://www.androidauthority.com/learn-kotlin-for-android-774599/
- https://github.com/Kotlin