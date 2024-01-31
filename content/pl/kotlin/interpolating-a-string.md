---
title:                "Interpolacja łańcuchów znaków"
date:                  2024-01-20T17:51:28.612652-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolacja łańcuchów znaków"

category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Interpolacja stringów to proces wstawiania dynamicznie wyliczanych wartości do łańcuchów znakowych. Programiści używają tego, aby ułatwić sobie budowanie dynamicznych tekstów bez potrzeby sklejania ich części.

## How to:
```Kotlin
fun main() {
    val name = "Ania"
    val age = 25
    val greeting = "Cześć, jestem $name i mam $age lat."

    println(greeting) // Wyświetla: Cześć, jestem Ania i mam 25 lat.

    // Można też wstawiać wyrażenia:
    val farewell = "Za rok będę miał ${age + 1} lat."
    println(farewell) // Wyświetla: Za rok będę miał 26 lat.
}
```

## Deep Dive
Interpolację stringów wprowadzono, aby ułatwić razem pracę z tekstami. W przeszłości programiści musieli ręcznie sklejać stringi przy użyciu operatorów plus (+), co było nie tylko uciążliwe, ale i mogło prowadzić do błędów. W Kotlinie interpolacja używa znaku dolara ($) do identyfikowania zmiennych oraz wyrażeń w nawiasach klamrowych do obliczeń.

Alternatywą dla interpolacji jest użycie metody `format` dostępnej w Javie i Kotlinie lub budowanie stringów przy pomocy klas `StringBuilder` czy `StringBuffer`. Te techniki są nadal używane tam, gdzie interpolacja jest niemożliwa lub niepraktyczna.

Implementacja interpolacji stringów w Kotlinie jest zoptymalizowana, aby minimalizować narzut związany z operacjami na stringach - kompilator zamienia interpolację na połączenie `StringBuilder` i jego metod.

## See Also
- [Kotlin Documentation on String Templates](https://kotlinlang.org/docs/basic-syntax.html#string-templates)
- [Kotlin Playground](https://play.kotlinlang.org/) - Interactive site to test Kotlin code snippets.
