---
title:                "Usuwanie cudzysłowów z ciągu znaków"
date:                  2024-01-26T03:41:58.833828-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usuwanie cudzysłowów z ciągu znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Usuwanie cudzysłowów ze stringa oznacza wycięcie wszelkich instancji znaków cudzysłowu, zarówno pojedynczych (' ') jak i podwójnych (" "), z danych tekstowych, z którymi pracujesz. Programiści często muszą to robić w celu oczyszczenia danych, przygotowania do dalszego przetwarzania lub gdy same cudzysłowy nie są istotne dla znaczenia danych.

## Jak to zrobić:

Oto prosty sposób, aby usunąć oba typy cudzysłowów ze stringa w Kotlinie:

```kotlin
fun removeQuotes(input: String): String {
    return input.replace("\"", "").replace("'", "")
}

fun main() {
    val stringWithQuotes = "Kotlin \"rocks\" it's 'cool'"
    val stringWithoutQuotes = removeQuotes(stringWithQuotes)
    println(stringWithoutQuotes) // Wyjście: Kotlin rocks its cool
}
```

A jeśli chcesz usunąć tylko jeden typ cudzysłowu, po prostu pomiń drugie wywołanie replace.

```kotlin
fun removeDoubleQuotes(input: String): String {
    return input.replace("\"", "")
}

fun removeSingleQuotes(input: String): String {
    return input.replace("'", "")
}

fun main() {
    val stringWithQuotes = "Kotlin \"rocks\" it's 'cool'"
    println(removeDoubleQuotes(stringWithQuotes)) // Wyjście: Kotlin rocks it's 'cool'
    println(removeSingleQuotes(stringWithQuotes)) // Wyjście: Kotlin "rocks" its cool
}
```

## Wnikliwa analiza

Historycznie, obsługa stringów i znaków ucieczki była podstawową częścią programowania, ponieważ tekst jest podstawowym sposobem, w jaki wchodzimy w interakcję z danymi. Cudzysłowy wewnątrz stringów czasami muszą być poprzedzone znakiem ucieczki (np. `"She said, \"Hi!\""`). Przetwarzając takie stringi, możesz potrzebować usunąć znaki ucieczki lub same cudzysłowy, aby uzyskać czystszy lub bardziej użyteczny tekst.

Alternatywy dla metody `replace` obejmują usuwanie oparte na wyrażeniach regularnych lub ręczne parsowanie stringa, znak po znaku. Jednak regex może być nadmiernym rozwiązaniem dla prostych operacji, a ręczne parsowanie jest mniej wydajne niż korzystanie z wbudowanych funkcji stringów. Funkcja `replace` w Kotlinie korzysta z leżącej u jej podstaw metody `String` `replace` Javy, która jest dobrze zoptymalizowana pod kątem wydajności.

Pod względem implementacji warto wspomnieć, że Kotlin jest interoperacyjny z Javą, więc w efekcie wszelkie operacje, które wykonujesz na stringach, są równie wydajne, jak byłyby w Javie. Podczas usuwania cudzysłowów ważne jest, aby być świadomym przypadków brzegowych, takich jak zagnieżdżone cudzysłowy, które mogą wymagać bardziej zaawansowanego podejścia, możliwie z wykorzystaniem wyrażeń regularnych lub biblioteki parsującej.

## Zobacz także

Aby uzyskać więcej kontekstu na temat obsługi stringów w Kotlinie, możesz sprawdzić oficjalną dokumentację:

- [Dokumentacja Stringów w Kotlinie](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)

Aby zagłębić się w wyrażenia regularne i parsowanie w Kotlinie:

- [Dokumentacja Kotlin Regex](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
