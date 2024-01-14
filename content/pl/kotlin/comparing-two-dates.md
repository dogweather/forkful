---
title:    "Kotlin: Porównywanie dwóch dat"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami w programowaniu musimy porównać dwa różne daty. Może to być potrzebne do sprawdzenia ważności ważnych dokumentów, ustalenia różnicy czasu między dwoma wydarzeniami lub po prostu do celów statystycznych. W tym blogu omówimy jak w prosty sposób porównać dwie daty w języku Kotlin.

## Jak To Zrobić

Porównywanie dwóch dat w języku Kotlin jest prostsze niż myślisz. Możemy wykorzystać metodę .compareTo(), która jest dostępna dla obiektów klasy Date. Sprawia to, że nasze porównanie jest nie tylko łatwe, ale także dokładne. Poniżej przedstawiamy kod, który można użyć do porównania dwóch dat:

```Kotlin
val date1 = Date(2020, 11, 1)
val date2 = Date(2021, 5, 10)

if (date1.compareTo(date2) < 0) {
    println("Date1 jest wcześniejsza niż date2.")
} else if (date1.compareTo(date2) > 0) {
    println("Date2 jest wcześniejsza niż date1.")
} else {
    println("Obie daty są takie same.")
}
```

Ten kod najpierw tworzy dwa obiekty daty, a następnie używa metody .compareTo(), aby porównać je ze sobą. Metoda ta zwraca wartość, która jest mniejsza niż 0, jeśli pierwsza data jest wcześniejsza od drugiej, większa niż 0, jeśli druga data jest wcześniejsza, i równa 0, jeśli obie są takie same. W naszym przykładzie jeśli date1 jest wcześniejsza niż date2, to zostanie wyświetlony odpowiedni komunikat.

## Wnikliwe Spostrzeżenia

Co jednak zrobić, jeśli chcemy porównać nie tylko daty, ale także czasy? W takim przypadku możemy stworzyć obiekty klasy LocalDateTime, które zawierają informacje o dacie i czasie, a następnie użyć metod .isBefore() lub .isAfter(), aby porównać je ze sobą. Poniżej przedstawiamy przykład:

```Kotlin
val dateTime1 = LocalDateTime.of(2020, 11, 1, 12, 0)
val dateTime2 = LocalDateTime.of(2020, 10, 31, 18, 30)

if (dateTime1.isBefore(dateTime2)) {
    println("DateTime1 jest wcześniejsza niż dateTime2.")
} else if (dateTime1.isAfter(dateTime2)) {
    println("DateTime2 jest wcześniejsza niż dateTime1.")
} else {
    println("Obie daty są takie same.")
}
```

Metody .isBefore() i .isAfter() zwracają wartość logiczną, więc można je wykorzystać bezpośrednio w instrukcji warunkowej.

## Zobacz także

- Dokumentacja języka Kotlin o klasie Date: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date/
- Dokumentacja języka Kotlin o klasie LocalDateTime: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-local-date-time/