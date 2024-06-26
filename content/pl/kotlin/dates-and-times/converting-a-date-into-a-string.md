---
date: 2024-01-20 17:37:04.570733-07:00
description: "Jak to zrobi\u0107: Kotlin pozwala na konwersj\u0119 daty do stringa\
  \ za pomoc\u0105 klasy `SimpleDateFormat`. Przyk\u0142ad."
lastmod: '2024-03-13T22:44:35.376646-06:00'
model: gpt-4-1106-preview
summary: "Kotlin pozwala na konwersj\u0119 daty do stringa za pomoc\u0105 klasy `SimpleDateFormat`."
title: "Konwersja daty na \u0142a\u0144cuch znak\xF3w"
weight: 28
---

## Jak to zrobić:
Kotlin pozwala na konwersję daty do stringa za pomocą klasy `SimpleDateFormat`. Przykład:

```Kotlin
import java.text.SimpleDateFormat
import java.util.*

fun formatDates(): String {
    val date = Date() // tworzy obiekt daty
    val dateFormat = SimpleDateFormat("dd/MM/yyyy HH:mm", Locale("pl", "PL"))
    return dateFormat.format(date) // konwersja daty do stringa
}

fun main() {
    println(formatDates()) // wyświetla sformatowaną datę
}
```

Jeśli uruchomimy powyższy kod, na przykład 10 kwietnia 2023 o 15:30, wynik powinien wyglądać tak:

```
10/04/2023 15:30
```

## Deep Dive
Przed wprowadzeniem `java.time` w Java 8, standardem był pakiet `java.util` i klasy takie jak `Date` oraz `SimpleDateFormat`. Kotlin, będąc językiem działającym na JVM, dzieli ten sam zestaw narzędzi.

Alternatywą dla `SimpleDateFormat` jest nowsze API `java.time`, dostępne od wersji Kotlin 1.3, które ułatwia manipulację czasem i jest bardziej odporny na błędy:

```Kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.Locale

fun formatDatesWithJavaTime(): String {
    val date = LocalDateTime.now()
    val formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm").withLocale(Locale("pl", "PL"))
    return date.format(formatter)
}
```

`DateTimeFormatter` jest bezpieczniejszy w użyciu, gdyż bierze pod uwagę kontekst takie jak strefy czasowe i wsparcie dla i18n (internacjonalizacje). Co więcej, unika błędów związanych z wielowątkowością, na które narażony jest `SimpleDateFormat`.

## Zobacz też
- [SimpleDateFormat documentation](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [DateTimeFormatter documentation](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Kotlin API docs](https://kotlinlang.org/api/latest/jvm/stdlib/)
- [Oracle's Date and Time Patterns guide](https://docs.oracle.com/javase/tutorial/i18n/format/simpleDateFormat.html)
