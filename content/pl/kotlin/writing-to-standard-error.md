---
title:                "Pisanie do standardowego błędu"
html_title:           "Kotlin: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Kotlin jest coraz popularniejszym językiem programowania zarówno wśród początkujących, jak i doświadczonych programistów. Jedną z przydatnych funkcji oferowanych przez Kotlin jest możliwość wypisywania wiadomości na standardowe wyjście błędów. W tym artykule dowiesz się, dlaczego warto korzystać z tej funkcji i jak jej używać.

## Jak używać standardowego wyjścia błędów w Kotlin

Za pomocą wbudowanej funkcji `System.err.println()` można wypisać wiadomość na standardowe wyjście błędów. Przykład poniżej pokazuje, jak użyć tej funkcji w celu wypisania informacji o błędzie do konsoli:

```Kotlin
fun main() {
    System.err.println("Wystąpił błąd!")
}
```

Przykładowy output:

```Kotlin
Wystąpił błąd!
```

Można również użyć wyrażenia `System.err.print()` jeśli nie chcemy, aby wypisywana informacja była automatycznie zakończona nową linią. Przykład:

```Kotlin
fun main() {
    System.err.print("Wystąpił błąd ")
    System.err.print("podczas odczytu pliku")
}
```

Przykładowy output:

```Kotlin
Wystąpił błąd podczas odczytu pliku
```

## Deep Dive

Funkcje `System.err.println()` i `System.err.print()` wykorzystują standardowe wyjście błędów, które jest oddzielone od standardowego wyjścia za pomocą specjalnego strumienia. Dzięki temu, w przypadku wystąpienia błędów, są one wypisywane niezależnie od innych informacji wyświetlanych na standardowym wyjściu. Jest to bardzo przydatne w przypadku, gdy chcemy otrzymywać informacje o błędach w sposób wyraźny i niezakłócony przez inne wiadomości.

## Zobacz także

Jeśli chcesz poznać więcej o możliwościach jakie oferuje język Kotlin, zapoznaj się z poniższymi artykułami:

- [Jak używać process builder w Kotlin](https://www.example.com/process-builder-kotlin)
- [Podstawowe operatory w Kotlin](https://www.example.com/basic-operators-kotlin)