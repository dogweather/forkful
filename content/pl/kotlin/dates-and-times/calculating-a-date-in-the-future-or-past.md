---
date: 2024-01-20 17:31:28.145140-07:00
description: "Jak to zrobi\u0107: Na pocz\u0105tek potrzebujemy prostego kodu. U\u017C\
  yjemy `LocalDate` z biblioteki `java.time`, kt\xF3ra jest dost\u0119pna w Kotlinie.\
  \ Tak to zrobisz."
lastmod: '2024-03-13T22:44:35.378589-06:00'
model: gpt-4-1106-preview
summary: "Na pocz\u0105tek potrzebujemy prostego kodu."
title: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci"
weight: 26
---

## Jak to zrobić:
Na początek potrzebujemy prostego kodu. Użyjemy `LocalDate` z biblioteki `java.time`, która jest dostępna w Kotlinie. Tak to zrobisz:

```Kotlin
import java.time.LocalDate
import java.time.temporal.ChronoUnit

fun main() {
    val today = LocalDate.now()
    val tenDaysLater = today.plusDays(10)
    val tenDaysBefore = today.minusDays(10)

    println("Dzisiaj jest: $today")
    println("Za 10 dni będzie: $tenDaysLater")
    println("10 dni temu było: $tenDaysBefore")
}
```

Wyjście (zależnie od aktualnej daty):
```
Dzisiaj jest: 2023-04-01
Za 10 dni będzie: 2023-04-11
10 dni temu było: 2023-03-22
```

## Głębsze spojrzenie:
Biblioteka `java.time` wprowadzona w Javie 8 (i dostępna w Kotlinie) to nowoczesne API do obchodzenia się z datą i czasem, który zastąpił starsze, mniej intuicyjne klasy jak `java.util.Date`. W Kotlinie możemy używać tej biblioteki bezpośrednio, albo z pomocą Kotlin extensions, które czynią kod nawet bardziej czytelnym.

Obecnie `java.time` jest standardem do pracy z datami w Javie oraz Kotlinie, ale istnieją też alternatywy jak Joda-Time – już jednak coraz rzadziej używana po wprowadzeniu nowego API.


Warte zastanowienia są też kwestie stref czasowych – `LocalDate` ignoruje strefy czasowe, więc jeśli działamy globalnie, możemy potrzebować `ZonedDateTime`. 

Dodatkowo, możemy manipulować datami odpowiadającymi różnym jednostkom czasu używając `plus` i `minus` z różnymi argumentami jak `ChronoUnit.WEEKS` czy `ChronoUnit.MONTHS`, ale pamiętaj o niuansach związanych ze zmianą czasu letniego/zimowego itp.

## Zobacz również:
- Dowiedz się więcej o `java.time` bezpośrednio z [dokumentacji Oracle](https://docs.oracle.com/javase/tutorial/datetime/).
- Przejrzyj kotlinową dokumentację dotyczącą pracy z datami na [stronie Kotlinlang](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.js/-date/).
- Przeczytaj o alternatywnej bibliotece Joda-Time na [ich oficjalnej stronie](https://www.joda.org/joda-time/).
