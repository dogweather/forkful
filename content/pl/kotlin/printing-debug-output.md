---
title:                "Wydruk danych debugowania"
html_title:           "Kotlin: Wydruk danych debugowania"
simple_title:         "Wydruk danych debugowania"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Wydruki debugowania to specjalne wiadomości, które programiści umieszczają w swoim kodzie w celu śledzenia wykonywania programu. Ułatwiają one zrozumienie, co dzieje się w kodzie i pomagają w identyfikacji błędów.

## Jak to zrobić:
Kotlin udostępnia dwa sposoby na drukowanie wyjścia debugowania: wywołanie metody `println()` lub użycie wbudowanej funkcji `debug()`. Oto przykładowy kod z wykorzystaniem obu metod:
```
val x = 5
println("wartość zmiennej x: $x") // wykorzystanie metody println()
debug("wartość zmiennej x: $x") // wykorzystanie funkcji debug()
```
Powyższy kod wyświetli na konsoli następujące wyjście:
```
wartość zmiennej x: 5
D/MyApp: wartość zmiennej x: 5
```

## Głębsza analiza:
Pierwsze drukowane wyjście debugowania pojawiło się w latach 70-tych w języku C. Pojawienie się wyspecjalizowanych narzędzi do debugowania takich jak GDB czy valgrind spowodowało, że wydruki debugowania stały się łatwiejsze i bardziej wydajne. Alternatywą dla drukowania wyjścia debugowania jest używanie breakpointów w debuggerze.

## Zobacz także:
Dokumentacja Kotlin dotycząca funkcji `debug()` <https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/debug.html>