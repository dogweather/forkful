---
title:                "Porównywanie dwóch dat"
html_title:           "Kotlin: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dat jest kluczowym elementem w wielu aplikacjach i systemach informatycznych. Poprawne porównanie dat jest niezbędne do wykrywania i rozwiązywania błędów, planowania czasu oraz wykonania operacji na plikach i bazach danych. Dlatego, znajomość sposobu porównywania dat w języku Kotlin jest niezwykle przydatna dla programistów.

## Jak to zrobić

Kotlin posiada wiele wbudowanych funkcji, które ułatwiają porównywanie dat. Jedną z nich jest metoda `compareTo()`, która porównuje dwie daty i zwraca wartość całkowitą w zależności od wyniku porównania. Przykładowe użycie tej metody wygląda następująco:

```Kotlin
val date1 = LocalDate.of(2021, 10, 1)
val date2 = LocalDate.of(2021, 10, 5)

val result = date1.compareTo(date2)
println(result) // wyświetli -4
```

Powyższy kod porównuje dwie daty: 1 października 2021 i 5 października 2021. Metoda `compareTo()` zwróciła wartość -4, co oznacza, że `date1` jest wcześniej niż `date2`.

Kotlin umożliwia również porównywanie dat z uwzględnieniem czasu. W tym celu można skorzystać z metody `compare()`, która przyjmuje dwa parametry - obiekty typu `LocalDateTime`. Przykład użycia:

```Kotlin
val dateTime1 = LocalDateTime.of(2021, 10, 1, 14, 30)
val dateTime2 = LocalDateTime.of(2021, 10, 1, 16, 45)

val result = dateTime1.compareTo(dateTime2)
println(result) // wyświetli -2
```

W tym przypadku porównywane są daty z uwzględnieniem czasu - 1 października 2021, godzina 14:30 i 1 października 2021, godzina 16:45. Metoda `compare()` zwróciła wartość -2, ponieważ `dateTime1` jest wcześniejsza niż `dateTime2`.

## Deep Dive

Głębsze poznanie sposobu porównywania dat w języku Kotlin wymaga zrozumienia, jak działają klasy związane z datami - `LocalDate` i `LocalDateTime`. Obie klasy są niezmienialne (immutable) i dostarczają wiele metod do porównywania dat.

Metoda `compareTo()` jest dostępna w obu klasach i porównuje daty na podstawie wyznaczonej kolejności. Jednak, dla `LocalDate` porównanie odbywa się tylko na podstawie daty, bez uwzględnienia czasu. Natomiast dla `LocalDateTime` uwzględnia również czas.

Kotlin również umożliwia porównywanie dat przy użyciu operatorów porównania (`<`, `<=`, `>`, `>=`) lub metody `equals()`. Przykład użycia operatora porównania:

```Kotlin
val date1 = LocalDate.of(2021, 10, 1)
val date2 = LocalDate.of(2021, 10, 5)

println(date1 < date2) // wyświetli true
```

Wszystkie wymienione metody zwracają wartości całkowite, dlatego można je wykorzystać w warunkach logicznych lub do sortowania list i kolekcji obiektów zawierających daty.

## Zobacz również

- Dokumentacja Kotlin na temat pracy z datami: https://kotlinlang.org/docs/dates.html
- Przykłady użycia metod do porównywania dat: https://developer.android.com/reference/java/time/LocalDateTime
- Porównywanie dat w języku Java: https://www.baeldung.com/java-compare-dates