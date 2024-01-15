---
title:                "भविष्य या भूतकाल में तारीख की गणना करना"
html_title:           "Kotlin: भविष्य या भूतकाल में तारीख की गणना करना"
simple_title:         "भविष्य या भूतकाल में तारीख की गणना करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Kyon
Agar aapko pata karna hai ki aapko kis din kisi event ko celebrate karna hai ya aapko ek certain date tak kaam karna hai, to aapko pichle aur aane waale dates ka calculation karna hoga. Isliye, dates ko calculate karne ki jarurat padti hai.

## Kaise Kare
Date ko calculate karne ke liye, Kotlin mein "Calendar" class ka istemal kiya ja sakta hai. Is class mein "add()" method ka use karke hum dates mein changes kar sakte hain. Yahan hamne ek example diya hai jo current date se 3 din aage ki date calculate karta hai.

```Kotlin
fun main() {
    var calendar = Calendar.getInstance()
    calendar.add(Calendar.DATE, 3)

    println("Aane waali date: ${calendar.time}")
}
```

Output:
```
Aane waali date: Wed Oct 13 12:45:38 IST 2021
```

Is code mein humne "add()" method mein "Calendar.DATE" aur "3" ka istemal kiya hai. "Calendar.DATE" current date ko represent karta hai aur "3" number of days bataata hai jinhe hum aane waali date mein add karna chahte hain.

## Deep Dive
Date ko calculate karne ke liye, hum "add()" method mein alag-alag constants use kar sakte hain jaise "Calendar.MONTH" ya "Calendar.YEAR". Yeh date ko calculate karne ke liye ek flexible aur easy way hai.

Ek aur tareeka date ka calculation karne ka hai "Calendar" class mein maujood "set()" method ka istemal karna. Is method se hum date ko set kar sakte hain aur phir use "add()" method ke jariye modify kar sakte hain.

## Dekhna Bhi
- [Kotlin Date and Time API](https://developer.android.com/reference/kotlin/java/util/Calendar)
- [Kotlin Reference for Calendar class](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-calendar/index.html)