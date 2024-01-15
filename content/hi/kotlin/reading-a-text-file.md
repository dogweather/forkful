---
title:                "एक पाठ फ़ाइल पढ़ना"
html_title:           "Kotlin: एक पाठ फ़ाइल पढ़ना"
simple_title:         "एक पाठ फ़ाइल पढ़ना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Kyu

Kisi bhi programmer ke liye text files ek bahut important aur pratishthit source of information hai. Text files me hum sirf text likh sakte hai, jiske wajah se wo bahut asaani se readable aur accessible ho jaate hai. Isliye, agar aapko kisi bhi programming language ko sikhna hai ya fir kisi project me work karna hai, toh text files ko padhna aur unme se data extract karke use karna bahut hi zaruri hai.

## Kaise Kare

```Kotlin
val file = File("path/to/file.txt")
file.forEachLine { println(it) }
```
Is coding example me humne ek text file ko read karke uske har line ko print kiya hai. `File()` function ko use karke hum file ka path specify kar sakte hai. Fir `forEachLine` function me hum `println` use karke file ke har line ko print kar sakte hai.

Is tarah se, aap Kotlin ka `File` class aur `forEachLine` function ka use karke text files ko bahut asani se read aur manipulate kar sakte hai.

## Deep Dive

Text files me data line by line format me store hota hai. Isliye, hum `forEachLine` function ka use karte hai taki hum file ke har line ko separately access kar sake. Agar aap `readText()` function ka use karte hai, toh pura file ek string ke roop me return ho jayega.

Hum `FileReader` class bhi use kar sakte hai text files ke read karne ke liye. `FileReader` class ka ek constructor hai jisme hum file ka path specify karke `readText()` function ka use kar sakte hai.

## Dekhiye Bhi

Agar aapko aur jaankari chahiye text files ke read karne ke baare me, toh neeche di gayi links par visit kare:
- https://kotlinlang.org/docs/reference/input-output.html
- https://www.tutorialspoint.com/kotlin/kotlin_read_write_files.htm
- https://www.baeldung.com/kotlin-read-file