---
title:                "Kotlin: Wydrukowanie wyników debugowania"
programming_language: "Kotlin"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasem, pisząc kod w języku Kotlin, zdarza się, że napotkamy na błędy lub niechciane wyniki. W takiej sytuacji przydatne jest wypisywanie informacji w celu zrozumienia, jak przebiega wykonanie programu. W tym wpisie dowiesz się, jak to zrobić.

## Jak To Zrobić

```Kotlin
fun main() {
    val x = 10
    val y = 5
    val result = x + y

    println("Wartość x: $x")
    println("Wartość y: $y")
    println("Wynik dodawania x i y: $result")
}
```

W powyższym kodzie wypisujemy wartości zmiennych `x` i `y`, a także wynik dodawania tych zmiennych. Dzięki temu możemy śledzić, jakie konkretne wartości zostały przypisane do zmiennych i zobaczyć, czy działanie jest zgodne z naszym oczekiwaniem. Można również wykorzystać wbudowaną funkcję `debug` w języku Kotlin, aby wyświetlać informacje o kolejnych wywołaniach funkcji w programie.

## Deep Dive

Wypisywanie informacji w celu debugowania kodu jest szczególnie przydatne przy większych projektach, w których trudno jest znaleźć konkretny błąd przez zwykłe czytanie kodu. W takiej sytuacji można wypisać wartości kilku zmiennych w różnych miejscach programu, aby zobaczyć, jak zmieniają się one podczas jego wykonania.

Należy jednak pamiętać, że wyświetlanie zbyt dużej ilości informacji może spowolnić działanie programu. Dlatego warto ograniczyć wypisywane informacje tylko do tych najważniejszych.

## Zobacz również

- [Oficjalna dokumentacja języka Kotlin](https://kotlinlang.org/docs/tutorials/kotlin-for-py/funcs-and-vals.html#in-line-debugging)
- [Tutorial o debuggingu w języku Kotlin](https://kodejava.org/how-do-i-do-inline-debugging-with-kotlin/)
- [Poradnik o debugowaniu kodu w języku Kotlin](https://www.baeldung.com/kotlin/debugging)