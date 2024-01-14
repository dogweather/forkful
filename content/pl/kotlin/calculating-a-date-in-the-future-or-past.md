---
title:    "Kotlin: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Programowanie jest fascynującą dziedziną, która pozwala nam rozwiązywać różnorodne problemy. Jednym z takich problemów może być obliczenie daty w przeszłości lub w przyszłości. W tym artykule przyjrzymy się, dlaczego warto nauczyć się tworzyć taki kod w języku Kotlin.

## Jak to zrobić

Programowanie w języku Kotlin jest proste i przyjemne, dzięki czemu obliczanie daty w przeszłości lub przyszłości również jest łatwe. Poniżej znajdziesz przykładowy kod, który pozwala na obliczenie daty w przyszłości i wyswietlenie jej w formacie "dd.MM.yyyy". 

```Kotlin 
import java.time.LocalDate

fun main() {
    val dzis = LocalDate.now()
    val dniDoDodania = 7
    val przyszlaData = dzis.plusDays(dniDoDodania.toLong())
    println("Data za 7 dni: ${przyszlaData.format(DateTimeFormatter.ofPattern("dd.MM.yyyy"))}")
}

```

Wynik:

```
Data za 7 dni: 29.12.2020
```

Powtórzmy teraz ten sam kod, ale tym razem dodajmy datę wstecz.

```Kotlin
import java.time.LocalDate

fun main() {
    val dzis = LocalDate.now()
    val dniDoOdejmowania = 7
    val przeszlaData = dzis.minusDays(dniDoOdejmowania.toLong())
    println("Data sprzed 7 dni: ${przeszlaData.format(DateTimeFormatter.ofPattern("dd.MM.yyyy"))}")
}

```

Wynik:

```
Data sprzed 7 dni: 15.12.2020
```

Jak widać, dzięki wykorzystaniu wbudowanych funkcji języka Kotlin oraz klasy LocalDate, obliczanie daty w przeszłości lub przyszłości jest bardzo proste.

## Głębszy wgląd

Teraz przejdźmy do rzeczy bardziej zaawansowanych. Język Kotlin oferuje wiele możliwości w zakresie obliczania dat. Możemy na przykład obliczyć różnicę między dwiema datami lub sprawdzić, czy dana data jest datą przestępną.

Aby obliczyć różnicę między dwiema datami, możemy użyć funkcji until(), która jest dostępna w klasie LocalDate. Poniżej przykład:

```Kotlin
import java.time.LocalDate

fun main() {
    val pierwszaData = LocalDate.of(2020, 5, 1)
    val drugaData = LocalDate.of(2020, 5, 10)
    val roznica = pierwszaData.until(drugaData).days
    println("Różnica między datami: $roznica dni")
}

```

Wynik:

```
Różnica między datami: 9 dni
```

Aby sprawdzić, czy dana data jest datą przestępną, możemy wykorzystać metodę isLeapYear(), która sprawdza, czy dany rok jest rokiem przestępnym. Poniżej przykład:

```Kotlin
import java.time.LocalDate

fun main() {
    val data = LocalDate.of(2020, 1, 1)
    if (data.isLeapYear()) {
        println("Rok ${data.year} jest rokiem przestępnym")
    } else {
        println("Rok ${data.year} nie jest rokiem przestępnym")
    }
}

```

Wynik:

```
Rok 2020 jest rokiem przestępnym
```

Jak widać, możliwości obliczania dat w języku Kotlin są szerokie i warto je poznać, aby jeszcze lepiej wykorzystywać ten język w swoich projektach.

## Zobacz również

- Dokumentacja języka Kotlin: https://kotlinlang.org/docs/reference/
- Informacje o klasie LocalDate: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date/
- Przykładowy projekt z wykorzystaniem oblicz