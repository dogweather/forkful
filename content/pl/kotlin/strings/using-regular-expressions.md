---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:33.541352-07:00
description: "Jak to zrobi\u0107: Aby sprawdzi\u0107, czy ci\u0105g znak\xF3w pasuje\
  \ do okre\u015Blonego wzorca w Kotlinie, mo\u017Cna u\u017Cy\u0107 metody `matches`\
  \ klasy `Regex`."
lastmod: '2024-03-13T22:44:35.354616-06:00'
model: gpt-4-0125-preview
summary: "Aby sprawdzi\u0107, czy ci\u0105g znak\xF3w pasuje do okre\u015Blonego wzorca\
  \ w Kotlinie, mo\u017Cna u\u017Cy\u0107 metody `matches` klasy `Regex`."
title: "Korzystanie z wyra\u017Ce\u0144 regularnych"
weight: 11
---

## Jak to zrobić:


### Podstawowe Dopasowywanie
Aby sprawdzić, czy ciąg znaków pasuje do określonego wzorca w Kotlinie, można użyć metody `matches` klasy `Regex`.

```kotlin
val pattern = "kotlin".toRegex()
val input = "Kocham kotlin"
val result = pattern.containsMatchIn(input)

println(result)  // Wynik: true
```

### Znajdowanie i Ekstrahowanie Części Ciągu Znaków
Jeśli chcesz znaleźć części ciągu znaków, które pasują do wzorca, Kotlin pozwala iterować po wszystkich dopasowaniach:

```kotlin
val datePattern = "\\d{2}/\\d{2}/\\d{4}".toRegex()
val input = "Dzisiejsza data to 07/09/2023."
val dates = datePattern.findAll(input)

for (date in dates) {
    println(date.value)
}
// Wynik: 07/09/2023
```

### Zastępowanie Tekstu
Zastępowanie części ciągu znaków, które pasują do wzorca, jest proste dzięki funkcji `replace`:

```kotlin
val input = "Nazwa użytkownika: user123"
val sanitizedInput = input.replace("\\d+".toRegex(), "XXX")

println(sanitizedInput)  // Wynik: Nazwa użytkownika: userXXX
```

### Dzielenie Ciągów Znaków
Podzielenie ciągu znaków na listę, używając wzorca regex jako delimitera:

```kotlin
val input = "1,2,3,4,5"
val numbers = input.split(",".toRegex())

println(numbers)  // Wynik: [1, 2, 3, 4, 5]
```

### Biblioteki Stron Trzecich: Kotest
[Kotest](https://github.com/kotest/kotest) to popularna biblioteka testująca Kotlin, która rozszerza wbudowane wsparcie dla regex w Kotlinie, szczególnie przydatne do walidacji w przypadkach testowych.

```kotlin
// Zakładając, że Kotest został dodany do twojego projektu
import io.kotest.matchers.string.shouldMatch

val input = "kotlin@test.com"
input shouldMatch "\\S+@\\S+\\.com".toRegex()

// Test zostanie zaliczony, jeśli input pasuje do wzorca e-mail.
```

Inkorporując wyrażenia regularne w swoje aplikacje Kotlin, możesz efektywnie wykonywać zaawansowane przetwarzanie tekstu. Niezależnie od tego, czy walidujesz dane wejściowe użytkownika, ekstrahujesz dane, czy transformujesz ciągi znaków, wzorce regex oferują solidne rozwiązanie.
