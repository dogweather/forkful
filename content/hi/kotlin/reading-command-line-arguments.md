---
title:                "कम्प्यूटर प्रोग्रामिंग में कमांड लाइन आर्ग्यूमेंट पढ़ना"
html_title:           "Kotlin: कम्प्यूटर प्रोग्रामिंग में कमांड लाइन आर्ग्यूमेंट पढ़ना"
simple_title:         "कम्प्यूटर प्रोग्रामिंग में कमांड लाइन आर्ग्यूमेंट पढ़ना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Kyon
Kai baar hume apne programs mein user se interact karne ki zarurat hoti hai, jaise input lena ya specific output display karna. Command line arguments read karne se hum user se interact kar sakte hai aur apne program ko versatile bana sakte hai. Isse humare programs ka use increase hota hai aur hume flexibility milti hai.

## Kaise Kare
Kotlin mein command line arguments read karne ke liye hum `args` array ka use karte hai. Isse hume terminal mein user dvara enter kiye gaye arguments ko access karne mein madad milti hai. Chaliye ek simple program dekhenge jaha hum command line arguments ko read karke unhe print karenge.

```Kotlin
fun main(args: Array<String>) {
    println("Welcome, your name is ${args[0]}") // assuming the first argument is the name
}
```
Agar hume is program ko run karne ke liye terminal mein `kotlin Main.kt John` command enter karna hai, toh output mein "Welcome, your name is John" print hoga. Is tarah hum user ke dwara enter kiye gaye arguments ko read kar sakte hai.

## Deep Dive
Agar hum terminal mein `kotlin Main.kt John 25 true` command enter karte hai, toh humare `args` array mein 3 elements honge - John, 25 aur true. Yaha par `args[1]` hume age aur `args[2]` hume boolean value return karega. Isse hum apne program mein dynamic input ki tarah bhi use kar sakte hai.

## Dekhiye Bhi
- [Kotlin Command Line Arguments](https://kotlinlang.org/docs/command-line.html#command-line-arguments)
- [Kotlin Command Line Programs](https://www.freecodecamp.org/news/how-to-program-in-kotlin-command-line-programs/)