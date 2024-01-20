---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "Fish Shell: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Konwertowanie tekstu na małe litery to proces zmiany wszelkich dużych liter w łańcuchu na ich małe odpowiedniki. Robimy tak, kiedy chcemy porównać dwa ciągi bez uwzględniania ich wielkości liter.

## Jak tego dokonać:

Żeby to zrobić, wykorzystamy wbudowaną funkcję Kotlin `toLowerCase()`. Przyjmuje ona stringa i zwraca kopię stringa z wszystkimi literami zamienionymi na małe.

```Kotlin
fun main() {
    val text = "KOTLin ProGrAmming"
    val lowerCaseText = text.toLowerCase()
    println(lowerCaseText)
}
```
Wyjście:

```Kotlin
kotlin programming
```

## Szeroki kontekst:

1. **Kontekst historyczny**: funkcja `toLowerCase()` istnieje od pierwszych wersji języków programowania, takich jak Java, co pokazuje jej trwałe znaczenie.
2. **Alternatywy**: w Kotlinie jest także `toLowerCase(Locale)`, który pozwala przekształcić string na  małe litery zgodnie z lokalnymi ustawieniami np. dla różnych języków.
3. **Szczegóły implementacji**: `toLowerCase()` działa na zasadzie przeglądania każdego znaku ciągu i sprawdzania, czy jest literą dużą - jeśli tak, jest zamieniany na małą literę.

## Zobacz także:

1. Oficjalna dokumentacja Kotlin (toLowerCase()): https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html
2. StackOverflow (discussing toLowerCase()): https://stackoverflow.com/questions/5054995/how-to-convert-lower-case-to-upper-case-while-ignore-non-alphabetic-characters
3. Kotlin String API: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/index.html