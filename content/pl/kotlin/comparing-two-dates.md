---
title:                "Porównywanie dwóch dat"
html_title:           "C++: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Porównywanie dwóch dat to proces ustalania, która data jest wcześniejsza, późniejsza lub czy obie są takie same. Programiści często potrzebują tego do sortowania, filtracji danych lub kontroli poprawności dat wprowadzonych przez użytkowników.

## Jak to zrobić:
Podstawowe porównanie dwóch dat w Kotlinie to pestka. Używamy do tego wbudowanej funkcji `isBefore`, `isAfter` lub `isEqual`.
```Kotlin
val data1 = LocalDate.parse("2020-01-01")
val data2 = LocalDate.parse("2020-01-02")

println(data1.isBefore(data2))  // wydrukuje: true
println(data1.isAfter(data2))   // wydrukuje: false
println(data1.isEqual(data2))   // wydrukuje: false
```

## Wgłębne spojrzenie
Porównywanie dat w programowaniu to nie nowość – jest niezbędne od początków informatyki. W Kotlinie mamy kilka alternatywnych metod. Możemy na przykład użyć metody `compareTo`:

```Kotlin
println(data1.compareTo(data2)) // Jeżeli data1 < data2, zwraca wartość ujemną; jeżeli data1 > data2, zwraca wartość dodatnią; jeżeli są równe, zwraca 0.
```
   
Zwróć uwagę, że te metody porównują tylko datę, a nie czas. Jeżeli chcesz porównać również godziny, minuty itd., musisz użyć klasy `LocalDateTime` albo `ZonedDateTime`.

Możemy zdecydować się na takie podejście, jeżeli operacje na datach są złożone i wymagają uwzględnienia stref czasowych.

## Zobacz też
Więcej szczegółów na temat porównywania dat w Kotlinie możesz znaleźć w tych źródłach:
