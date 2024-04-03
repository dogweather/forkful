---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:49.515398-07:00
description: "Jak to zrobi\u0107: W Kotlinie napisy mog\u0105 by\u0107 zamieniane\
  \ na wielkie litery za pomoc\u0105 standardowych funkcji biblioteki, bez potrzeby\
  \ korzystania z bibliotek\u2026"
lastmod: '2024-03-13T22:44:35.347360-06:00'
model: gpt-4-0125-preview
summary: "W Kotlinie napisy mog\u0105 by\u0107 zamieniane na wielkie litery za pomoc\u0105\
  \ standardowych funkcji biblioteki, bez potrzeby korzystania z bibliotek firm trzecich."
title: "Zamiana liter na wielkie w \u0142a\u0144cuchu znak\xF3w"
weight: 2
---

## Jak to zrobić:
W Kotlinie napisy mogą być zamieniane na wielkie litery za pomocą standardowych funkcji biblioteki, bez potrzeby korzystania z bibliotek firm trzecich. Podejście Kotlina do obsługi ciągów znaków sprawia, że te operacje są proste i zwięzłe.

### Zamiana całego ciągu na wielkie litery:
```kotlin
val message = "hello, world!"
val capitalizedMessage = message.uppercase()

println(capitalizedMessage) // Wynik: HELLO, WORLD!
```

### Zamiana tylko pierwszej litery na wielką:
Od Kotlin 1.5, funkcja `capitalize()` jest przestarzała i zastąpiona przez połączenie `replaceFirstChar` i wyrażenia lambda, które sprawdza, czy jest to mała litera, aby przekształcić ją na wielką.

```kotlin
val greeting = "hello, world!"
val capitalizedGreeting = greeting.replaceFirstChar {
    if (it.isLowerCase()) it.titlecase() else it.toString()
}

println(capitalizedGreeting) // Wynik: Hello, world!
```

To podejście zachowuje resztę zdania w jego oryginalnej formie, zmieniając tylko pierwszą literę na wielką.
