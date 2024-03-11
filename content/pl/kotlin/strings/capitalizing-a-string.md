---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:49.515398-07:00
description: "Zamiana pierwszej litery ci\u0105gu znak\xF3w na wielk\u0105 (kapitalizacja)\
  \ w programowaniu polega na konwersji pierwszego znaku ci\u0105gu na wielk\u0105\
  \ liter\u0119, je\u015Bli ju\u017C\u2026"
lastmod: '2024-03-11T00:14:08.528158-06:00'
model: gpt-4-0125-preview
summary: "Zamiana pierwszej litery ci\u0105gu znak\xF3w na wielk\u0105 (kapitalizacja)\
  \ w programowaniu polega na konwersji pierwszego znaku ci\u0105gu na wielk\u0105\
  \ liter\u0119, je\u015Bli ju\u017C\u2026"
title: "Zamiana liter na wielkie w \u0142a\u0144cuchu znak\xF3w"
---

{{< edit_this_page >}}

## Co i dlaczego?

Zamiana pierwszej litery ciągu znaków na wielką (kapitalizacja) w programowaniu polega na konwersji pierwszego znaku ciągu na wielką literę, jeśli już taka nie jest. Jest to przydatne do formatowania danych wejściowych użytkownika lub wyświetlania tekstów w interfejsie użytkownika w bardziej ustandaryzowany lub przyjazny dla człowieka sposób. Programiści wykonują tę operację, aby zapewnić spójność danych lub spełnić konkretne wymagania dotyczące formatowania w swoich aplikacjach.

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
