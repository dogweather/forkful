---
title:                "Łączenie łańcuchów znaków"
date:                  2024-01-20T17:35:06.973803-07:00
model:                 gpt-4-1106-preview
simple_title:         "Łączenie łańcuchów znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Łączenie łańcuchów znaków (stringów) to po prostu ich sklejanie. Czynimy to, by stworzyć zdania, wiadomości czy dynamicznie budowane wartości tekstowe w naszych aplikacjach.

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
