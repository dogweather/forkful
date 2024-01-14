---
title:    "Kotlin: Porównywanie dwóch dat"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dwóch dat w programowaniu jest niezwykle ważnym krokiem w wielu aplikacjach. Często musimy ustalić, która data jest wcześniejsza lub późniejsza, aby podjąć odpowiednie działania. W tym wpisie dowiesz się, jak porównywać daty w języku Kotlin.

## Jak to zrobić

Aby porównać dwie daty w Kotlinie, musimy najpierw utworzyć obiekty typu `LocalDate` reprezentujące te daty. Następnie możemy użyć metody `isAfter()` lub `isBefore()` w celu porównania dat. Poniżej znajduje się przykładowy kod, który pokazuje, jak to zrobić:

```Kotlin
val date1 = LocalDate.of(2021, 2, 15)
val date2 = LocalDate.of(2021, 2, 28)

println(date1.isBefore(date2)) // Output: true
```

W powyższym przykładzie utworzyliśmy dwa obiekty typu `LocalDate` - `date1` reprezentujący datę 15 lutego 2021 roku i `date2` reprezentujący datę 28 lutego 2021 roku. Następnie wywołaliśmy metodę `isBefore()` na `date1` i przekazaliśmy `date2` jako argument. Metoda ta zwraca wartość logiczną `true`, ponieważ data 15 lutego jest wcześniejsza niż data 28 lutego.

Możemy również porównywać daty na podstawie precyzji, np. tylko roku lub tylko miesiąca. W tym celu możemy użyć metod `isAfterYear()` lub `isBeforeMonth()`. Poniżej przedstawiamy przykład kodu:

```Kotlin
val date1 = LocalDate.of(2021, 2, 15)
val date2 = LocalDate.of(2021, 5, 1)

println(date1.isAfterYear(date2)) // Output: false
println(date2.isBeforeMonth(date1)) // Output: false
```

## Głębsze zagadnienia

Aby lepiej zrozumieć, jak porównywać daty w Kotlinie, warto zapoznać się z koncepcją ``ChronoLocalDate``, która jest interfejsem, który implementują m.in. `LocalDate`. Ten interfejs zawiera wiele przydatnych metod, które możemy wykorzystać do porównywania dat.

W powyższym przykładzie używaliśmy metod `isBefore()` i `isAfter()`, ale istnieje także metoda `compareTo()`, która zwraca liczbę całkowitą reprezentującą wynik porównania. Jeśli pierwsza data jest wcześniejsza niż druga, wynik będzie ujemny, jeśli są równe - wynik będzie równy 0, a jeśli pierwsza data jest późniejsza niż druga - wynik będzie dodatni.

```
val date1 = LocalDate.of(2021, 2, 15)
val date2 = LocalDate.of(2021, 2, 28)
val date3 = LocalDate.of(2021, 2, 15)

println(date1.compareTo(date2)) // Output: -1 (jest wcześniejsza)
println(date1.compareTo(date3)) // Output: 0 (są równe)
println(date2.compareTo(date1)) // Output: 1 (jest późniejsza)
```

## Zobacz też

- Dokumentacja języka Kotlin: https://kotlinlang.org/docs/home.html
- Porównywanie dat w języku Java: https://www.baeldung.com/java-compare-dates