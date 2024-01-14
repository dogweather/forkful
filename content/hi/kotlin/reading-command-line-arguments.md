---
title:                "Kotlin: कम्प्यूटर प्रोग्रामिंग पर लेख: कमांड लाइन आर्ग्यूमेंट्स को पढ़ना।"
simple_title:         "कम्प्यूटर प्रोग्रामिंग पर लेख: कमांड लाइन आर्ग्यूमेंट्स को पढ़ना।"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Kyu

Command line arguments hamare Kotlin programs ke liye ek bahut hi zaroori feature hai. Isse hum apne program ko dynamic bana sakte hai aur user se input le sakte hai. Agar aap apne Kotlin programming skills ko improve karna chahte hai, to command line arguments padhna aapke liye bahut helpful hoga.

# Kaise

Command line arguments ko read karne ke liye hum `args` array ka use karte hai, jisme program ke arguments stored hote hai. Chaliye ek example dekhte hai:

```Kotlin
fun main(args: Array<String>) {
    println("Welcome ${args[0]}!")
}
```

Is code mein humne bas ek argument ko print kiya hai, lekin aap isse further expand kar sakte hai apne requirements ke according. Agar aap multiple arguments hai, toh aap `args.size` property ka use karke unhe loop mein bhi access kar sakte hai.

# Deep Dive

Command line arguments mein hum usually user input ko ek string ki form mein lete hai. Isliye, inhe typecast karna necessary ho sakta hai. Agar aap chahte hai ki user apne input ko numbers ki form mein de, toh aap `toInt()` ya `toDouble()` methods ka use kar sakte hai.

```Kotlin
fun main(args: Array<String>) {
    val num1 = args[0].toInt()
    val num2 = args[1].toInt()
    // perform calculations
}
```

Agar aapke program mein sensitive information input kiya jata hai, toh aap `readLine()` ka use karke user se input le sakte hai. Isse information safe rehti hai aur command line arguments ki tarah visible nahi hoti.

# Dekhiye Bhi

- [Basics of Kotlin Programming](https://medium.com/@vicboma1/basic-kotlin-command-line-arguments-accepting-6109a46d39df)
- [Official Kotlin Documentation for Command Line Arguments](https://kotlinlang.org/docs/tutorials/command-line.html#command-line-arguments)