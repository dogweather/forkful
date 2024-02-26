---
date: 2024-01-20 17:51:28.612652-07:00
description: "Interpolacja string\xF3w to proces wstawiania dynamicznie wyliczanych\
  \ warto\u015Bci do \u0142a\u0144cuch\xF3w znakowych. Programi\u015Bci u\u017Cywaj\u0105\
  \ tego, aby u\u0142atwi\u0107 sobie budowanie\u2026"
lastmod: '2024-02-25T18:49:33.714760-07:00'
model: gpt-4-1106-preview
summary: "Interpolacja string\xF3w to proces wstawiania dynamicznie wyliczanych warto\u015B\
  ci do \u0142a\u0144cuch\xF3w znakowych. Programi\u015Bci u\u017Cywaj\u0105 tego,\
  \ aby u\u0142atwi\u0107 sobie budowanie\u2026"
title: "Interpolacja \u0142a\u0144cuch\xF3w znak\xF3w"
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
