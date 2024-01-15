---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Kotlin: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, jak obliczyć datę w przyszłości lub przeszłości? Może potrzebujesz określić datę wygaśnięcia umowy lub dowiedzieć się, kiedy odbędzie się ważne wydarzenie? W tym artykule opowiem Ci, jak to zrobić w języku Kotlin.

## Jak to zrobić

Aby obliczyć datę w przyszłości lub przeszłości, możemy skorzystać z klasy LocalDate z biblioteki standardowej języka Kotlin. Spójrzmy na poniższy przykład:

```Kotlin
val currentDate = LocalDate.now()
val futureDate = currentDate.plusYears(2).plusMonths(3)
val pastDate = currentDate.minusDays(10)
println("Dziś jest $currentDate, za 2 lata i 3 miesiące będzie $futureDate, a 10 dni temu było $pastDate.")
```

W powyższym kodzie najpierw tworzymy obiekt klasy LocalDate o nazwie "currentDate", reprezentujący aktualną datę. Następnie, wykorzystując metodę "plusYears" dodajemy 2 lata do tej daty, a następnie 3 miesiące za pomocą metody "plusMonths". W podobny sposób można odejmować dni, wykorzystując metodę "minusDays". Na koniec drukujemy wynik w czytelnej formie.

## Deep Dive

Klasa LocalDate zawiera wiele przydatnych metod, pozwalających na manipulację datami w różnych formatach. W przykładzie powyżej wykorzystaliśmy metody "plusYears", "plusMonths" i "minusDays", ale istnieje także wiele innych takich jak "plusWeeks", "plusHours" czy "minusMinutes". Dodatkowo, klasa ta umożliwia także operacje na dniach tygodnia oraz porównywanie dat.

## Zobacz też

- Dokumentacja klasy LocalDate w języku Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-local-date/
- Poradnik na temat wykorzystania dat w Kotlinie: https://www.baeldung.com/kotlin/dates
- Przykłady i wskazówki dla programistów: https://kotlinlang.org/docs/datetime.html