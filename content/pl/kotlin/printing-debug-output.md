---
title:                "Wydrukowanie wyników błędów"
html_title:           "Kotlin: Wydrukowanie wyników błędów"
simple_title:         "Wydrukowanie wyników błędów"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zdarzyło ci się mieć problem z odnalezieniem błędu w swoim kodzie? Czy traciłeś dużo czasu na debugowanie? Drukowanie debugowego wyjścia może być nieocenionym narzędziem w takich sytuacjach. Pozwala ono na wyświetlenie wartości zmiennych podczas działania programu, ułatwiając odnajdywanie błędów i poprawianie kodu.

## Jak to zrobić

Aby wyświetlić debugowe wyjście, możemy użyć jednej z dwóch funkcji wbudowanych w Kotlin: `print()` lub `println()`. Pierwsza z nich wyświetli tekst bez dodania znaku nowej linii, natomiast druga – z dodaniem.

Przykład użycia funkcji `print()`:

```Kotlin
val name = "Jan"
print("Witaj, $name!") // Wyświetli "Witaj, Jan!"
```

Przykład użycia funkcji `println()`:

```Kotlin
val age = 25
println("Masz $age lat.") // Wyświetli "Masz 25 lat." i przejdzie do nowej linii.
```

Możemy również wyświetlić więcej niż jedną zmienną, stosując operator `+`, lub można użyć funkcji `print()` lub `println()` wielokrotnie.

Przykład użycia operatora `+`:

```Kotlin
val number1 = 5
val number2 = 10
println("Suma $number1 + $number2 to ${number1 + number2}.") // Wyświetli "Suma 5 + 10 to 15."
```

## Głębszy zanurzenie

Warto pamiętać, że funkcje `print()` i `println()` mogą przyjmować różne typy danych, takie jak stringi, liczby czy zmienne. Możemy również używać znaków specjalnych, takich jak znak nowej linii `\n` lub znak tabulacji `\t`. Możemy również skorzystać z ferramentów deweloperskich takich jak debugger, który pozwala na precyzyjne śledzenie wartości zmiennych w trakcie działania programu.

## Zobacz także

- [Oficjalna strona języka Kotlin](https://kotlinlang.org/)
- [Dokumentacja funkcji `print()` i `println()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/print.html)
- [Poradnik programowania w języku Kotlin](https://kotlinlang.org/docs/getting-started.html)