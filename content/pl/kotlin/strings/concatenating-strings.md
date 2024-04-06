---
date: 2024-01-20 17:35:06.973803-07:00
description: "How to: (Jak to zrobi\u0107:) Dawniej, kiedy pami\u0119\u0107 by\u0142\
  a na wag\u0119 z\u0142ota, ka\u017Cdy znak mia\u0142 znaczenie. Dlatego zwyk\u0142\
  y plus (`+`) sta\u0142 si\u0119 standardem w wielu\u2026"
lastmod: '2024-04-05T22:50:49.674403-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Dawniej, kiedy pami\u0119\u0107 by\u0142a na wag\u0119\
  \ z\u0142ota, ka\u017Cdy znak mia\u0142 znaczenie."
title: "\u0141\u0105czenie \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 3
---

## How to: (Jak to zrobić:)
```kotlin
fun main() {
    val hello = "Cześć"
    val world = "Świat"
    val exclamation = "!"

    // Używanie operatora plus (+)
    val greeting1 = hello + ", " + world + exclamation
    println(greeting1) // Wyjście: Cześć, Świat!

    // Używanie string templates (wzorców łańcuchów znaków)
    val greeting2 = "$hello, $world$exclamation"
    println(greeting2) // Wyjście: Cześć, Świat!

    // Używanie funkcji joinToString()
    val words = listOf(hello, world, exclamation)
    val greeting3 = words.joinToString(separator = ", ")
    println(greeting3) // Wyjście: Cześć, Świat, !
}
```

## Deep Dive (W głąb tematu)
Dawniej, kiedy pamięć była na wagę złota, każdy znak miał znaczenie. Dlatego zwykły plus (`+`) stał się standardem w wielu językach programowania, wliczając w to Kotlin. Natomiast Kotlin, mając swoje korzenie w Javie, wnosi zaawansowane metody, takie jak string templates, które czynią kod bardziej czytelnym.

Inne metody to `StringBuilder`, wydajne przy dużym łączeniu znaków, oraz metody bibliotek zewnętrznych.

Należy pamiętać, że w Kotlin + jest przeładowane i tak naprawdę korzysta z `StringBuildera` pod spodem, gdy składamy więcej niż dwa łańcuchy. To elegancka kombinacja prostoty i wydajności.
