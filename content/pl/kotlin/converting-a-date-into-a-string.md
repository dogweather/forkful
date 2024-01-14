---
title:                "Kotlin: Konwertowanie daty na ciąg znaków"
simple_title:         "Konwertowanie daty na ciąg znaków"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Dlaczego warto przekonwertować datę na ciąg znaków?

Konwersja daty na ciąg znaków jest niezbędnym krokiem w wielu projektach programistycznych. Dla przykładu, może to być potrzebne do wyświetlenia daty w czytelnej formie dla użytkowników, lub do zapisania daty w odpowiednim formacie w bazie danych. W tym artykule dowiesz się, jak w prosty sposób przekonwertować datę na ciąg znaków w języku Kotlin.

## Jak to zrobić?

Często zdarza się, że konwersja daty na ciąg znaków wymaga użycia odpowiedniego formatu daty. W języku Kotlin możemy wykorzystać funkcję `format()` klasy `SimpleDateFormat` do przekonwertowania daty na ciąg znaków w wybranym formacie. Poniżej przedstawione są przykładowe kody oraz odpowiadające im wyniki:

```Kotlin
val currentDate = Date() // bieżąca data
val dateFormat = SimpleDateFormat("dd/MM/yyyy") // format daty
val dateString = dateFormat.format(currentDate) // konwersja daty na ciąg znaków w podanym formacie
println(dateString) // wyświetlenie wyniku: 20/10/2021
```

W powyższym przykładzie użyliśmy formatu "dd/MM/yyyy", ale istnieje wiele innych możliwych formatów, które mogą być wykorzystane w zależności od potrzeb.

## Głębsze zagadnienia

Podczas konwersji daty na ciąg znaków, często spotykamy się z problemem lokalizacji. Na przykład, format daty może być różny dla użytkowników w różnych krajach. W celu rozwiązania tego problemu, można użyć klasy `Locale` w połączeniu z funkcją `format()`.

Przykładowy kod:

```Kotlin
val currentDate = Date()
val dateFormat = SimpleDateFormat("MM/dd/yyyy", Locale("pl", "PL")) // ustawienie lokalizacji na język polski
val dateString = dateFormat.format(currentDate) // konwersja daty na ciąg znaków
println(dateString) // wyświetlenie wyniku: 10/20/2021
```

W tym przypadku, format daty został przetłumaczony na polski, a wynik wyświetla się w formacie "MM/dd/yyyy".

## Zobacz także

- Dokumentacja klasy SimpleDateFormat w języku Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-simple-date-format/
- Przewodnik po języku Kotlin: https://kotlinlang.org/docs/basic-syntax.html