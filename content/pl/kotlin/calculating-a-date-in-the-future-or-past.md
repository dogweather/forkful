---
title:                "Kotlin: Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Programowanie może być trudne, ale czasem warto wykorzystać je do obliczania dat w przeszłości lub przyszłości. Przydatne może to być na przykład podczas pracy z aplikacjami kalendarza lub budowania narzędzi do planowania czasu.

## Jak to zrobić

Aby obliczyć datę w przeszłości lub przyszłości w języku Kotlin, należy wykorzystać klasę `LocalDate` z pakietu `java.time`. Poniżej przedstawimy przykłady kodu, jak wykorzystać tę klasę do obliczenia daty z pominięciem różnych czynników jak np. uwzględnianie dnia tygodnia czy ilości dni w miesiącu.

### Obliczanie daty w przeszłości

W poniższym przykładzie obliczymy datę 30 dni temu od dzisiejszej daty:

```Kotlin
val today = LocalDate.now() // dzisiejsza data
val pastDate = today.minusDays(30) // obliczenie daty 30 dni temu
println(pastDate) // zostanie wyświetlona data sprzed 30 dni
```

### Obliczanie daty w przyszłości

Natomiast w tym przykładzie obliczymy datę 2 tygodnie w przyszłości od dzisiejszej daty:

```Kotlin
val today = LocalDate.now() // dzisiejsza data
val futureDate = today.plusWeeks(2) // obliczenie daty 2 tygodnie w przyszłości
println(futureDate) // zostanie wyświetlona data za 2 tygodnie
```

## Głębszy wgląd

Klasa `LocalDate` posiada również wiele innych przydatnych metod do obliczania dat w przeszłości i przyszłości, takich jak `minusMonths`, `minusYears`, `plusMonths` czy `plusYears`. Dodatkowo, możliwe jest również uwzględnienie dnia tygodnia przy obliczeniach, wykorzystując metodę `minus` lub `plus` z parametrem określającym ilość dni.

## Zobacz również

- [Dokumentacja języka Kotlin dla klasy LocalDate](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time.-local-date/) 
- [Poradnik o obliczaniu dat z wykorzystaniem pakietu java.time w języku Kotlin](https://dev.to/mkraska/how-to-use-java-time-with-kotlin-4d6n)