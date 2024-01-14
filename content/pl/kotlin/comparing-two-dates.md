---
title:                "Kotlin: Porównywanie dwóch dat"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu występuje potrzeba porównywania dat. Może to być przydatne w przypadku tworzenia aplikacji z wykorzystaniem czasu, monitorowania okresów wygaśnięcia lub wykonania określonej operacji na podstawie daty. W tym wpisie dowiesz się, jak porównywać dwie daty w języku Kotlin.

## Jak to zrobić

Aby porównać dwie daty w języku Kotlin, możemy wykorzystać metodę compareTo(). Poniżej znajduje się przykładowy kod w języku Kotlin używający tej metody:

```Kotlin
val date1 = LocalDateTime.of(2021, 8, 25, 13, 30)
val date2 = LocalDateTime.of(2021, 6, 10, 9, 45)

if(date1.compareTo(date2) > 0){
    println("Date 1 is after Date 2")
}
else if(date1.compareTo(date2) < 0){
    println("Date 1 is before Date 2")
}
else{
    println("Date 1 is equal to Date 2")
}
```

W tym przykładzie tworzymy dwie zmienne zawierające daty i porównujemy je za pomocą metody compareTo(). Jeśli pierwsza data jest po drugiej, porównanie zwróci wartość większą od zera. Jeśli druga data jest przed pierwszą, zwrócona zostanie wartość mniejsza od zera. W przypadku gdy obie daty są jednakowe, metoda zwróci wartość równą zero.

Możemy również porównywać daty na podstawie innych wartości, takich jak rok, miesiąc, dzień czy godzina. W tym celu możemy wykorzystać inne metody jak .isAfter(), .isBefore() czy .isEqual(). Przykład takiego porównania można znaleźć poniżej:

```Kotlin
val date1 = LocalDate.of(2021, 8, 25)
val date2 = LocalDate.of(2021, 6, 10)

if(date1.isAfter(date2)){
    println("Date 1 is after Date 2")
}
else if(date1.isBefore(date2)){
    println("Date 1 is before Date 2")
}
else{
    println("Date 1 is equal to Date 2")
}
```

## Głębszy zanurzenie

Klasa LocalDate w języku Kotlin oferuje wiele funkcji, które umożliwiają porównywanie dat na różne sposoby. Oprócz metod wspomnianych wcześniej, mamy również do dyspozycji inne, takie jak .isLeapYear(), .plusDays() czy .minusMonths(). Możemy również tworzyć daty z wykorzystaniem innych typów danych, takich jak String czy Long. Szczegółowe informacje na temat porównywania dat w języku Kotlin można znaleźć w dokumentacji na oficjalnej stronie internetowej.

## Zobacz także

- Dokumentacja języka Kotlin: https://kotlinlang.org/docs/datetime.html
- Porównywanie dat w języku Java: https://www.baeldung.com/java-compare-dates
- Przykładowe zadania z porównywania dat w języku Kotlin: https://www.testdome.com/d/java-interview-questions/4