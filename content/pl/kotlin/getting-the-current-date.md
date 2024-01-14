---
title:    "Kotlin: Uzyskiwanie aktualnej daty"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Czas jest nieustającym elementem naszego życia, który jest kluczowy w podejmowaniu decyzji oraz planowaniu działań. W programowaniu, często potrzebujemy obecnej daty, aby wykonać określone operacje. W tym artykule dowiesz się, jak w prosty sposób uzyskać aktualną datę w języku Kotlin.

## Jak to zrobić

Aby uzyskać aktualną datę w języku Kotlin, możemy skorzystać z klasy `LocalDate` z pakietu `java.time`. Najpierw musimy zaimportować ten pakiet do naszego projektu. Następnie, wewnątrz bloku `main`, możemy utworzyć nowy obiekt klasy `LocalDate` i przypisać mu aktualną datę.

```Kotlin
import java.time.LocalDate

fun main() {
    val currentDate = LocalDate.now()
    println("Aktualna data to: $currentDate")
}
```

Po uruchomieniu programu, w konsoli powinna pojawić się aktualna data w formacie `RRRR-MM-DD`, na przykład `2021-09-01`.

Możemy również wyświetlić tylko określone elementy daty, na przykład rok, miesiąc lub dzień. Służą do tego różne metody, takie jak `getYear()`, `getMonthValue()` czy `getDayOfMonth()`.

```Kotlin
import java.time.LocalDate

fun main() {
    val currentDate = LocalDate.now()
    val year = currentDate.getYear()
    val month = currentDate.getMonthValue()
    val day = currentDate.getDayOfMonth()
    println("Jest $day ${month}a, $year roku.")
}
```
Wynikiem tego kodu będzie: `Jest 1 września, 2021 roku.`

## Deep Dive

Obiekt `LocalDate` posiada wiele przydatnych metod, które pozwalają m.in. na porównywanie dat, dodawanie i odejmowanie dni czy wyznaczanie ilości dni w danym miesiącu. Pełną listę metod można znaleźć w dokumentacji języka Kotlin.

Ponadto, istnieje także możliwość tworzenia bardziej szczegółowych obiektów daty, np. `LocalDateTime` lub `ZonedDateTime`, które pozwolą nam również na manipulowanie czasem.

## Zobacz także

- Dokumentacja języka Kotlin: https://kotlinlang.org/docs/home.html
- Pakiet `java.time`: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
- Przykładowy kod: https://github.com/vogella/KotlinTutorials/tree/master/src