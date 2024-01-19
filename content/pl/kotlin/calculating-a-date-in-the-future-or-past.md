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

## Co & Dlaczego?

Obliczanie daty w przyszłości lub przeszłości to proces odpowiadania na pytania typu: "Jaka będzie data za 7 dni?" albo "Jaka była data 4 miesiące temu?". Programiści robią to w celach takich jak harmonogramowanie zadań czy tworzenie czasowych znaczników (timestamps).

## Jak to zrobić:

W Kotlinie możemy użyć klasy `LocalDate` z biblioteki `java.time` do manipulacji datami. Oto krótki przykład:

```Kotlin
import java.time.LocalDate

fun main() {
    val dzisiaj = LocalDate.now()
    val przyszlosc = dzisiaj.plusDays(7)
    val przeszlosc = dzisiaj.minusMonths(4)

    println("Dzisiaj: $dzisiaj")
    println("Przyszłość: $przyszlosc")
    println("Przeszłość: $przeszlosc")
}
```

Wynik:

```
Dzisiaj: 2022-04-01
Przyszłość: 2022-04-08
Przeszłość: 2021-12-01
```

## Pogłębiona analiza:

Historycznie, obliczanie dat bywało złożonym zadaniem, szczególnie z powodu ruchomych dni świątecznych, skomplikowanych reguł dotyczących roku przestępnego, itp. Nowoczesne języki programowania jak Kotlin ułatwiają ten proces przez wbudowane biblioteki.

Alternatywą dla `java.time.LocalDate` jest `Calendar` z Java, ale jest mniej wygodny i intuicyjny. `LocalDate` jest niezmienniczy (immutable) i bezpieczny do użycia w środowiskach wielowątkowych.

Jeśli chodzi o szczegóły implementacji, `plusDays()` i `minusMonths()` zwracają nowe instancje `LocalDate`. Obiekt oryginalny pozostaje niezmieniony.

## Zobacz także:

1. [Dokumentacja `java.time.LocalDate`](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
2. [Porównanie bibliotek dat i czasu w Javie](https://www.baeldung.com/java-8-date-time-intro)
3. [Tutorial do pakietu `java.time`](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)