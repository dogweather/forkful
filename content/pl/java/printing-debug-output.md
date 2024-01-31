---
title:                "Drukowanie komunikatów debugowania"
date:                  2024-01-20T17:52:43.593339-07:00
model:                 gpt-4-1106-preview
simple_title:         "Drukowanie komunikatów debugowania"

category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Drukowanie informacji debugowych to sposób na wypisywanie danych pomagających zrozumieć działanie kodu. Robimy to, aby szybko znaleźć i naprawić błędy.

## How to: (Jak to zrobić:)
Użyj `System.out.println()` dla prostego debugowania czy `System.err.println()` dla błędów. Oto przykład:

```java
public class DebugExample {
    public static void main(String[] args) {
        int sum = 0;
        for (int i = 0; i <= 5; i++) {
            sum += i;
            System.out.println("i=" + i + ", sum=" + sum); // Debug output
        }
        System.err.println("Completed with sum=" + sum); // Error output
    }
}
```

Wyjście:
```
i=0, sum=0
i=1, sum=1
i=2, sum=3
i=3, sum=6
i=4, sum=10
i=5, sum=15
Completed with sum=15
```

## Deep Dive (Dogłębna analiza):
W latach 60., kiedy komputery były duże i kosztowne, debugowanie odbywało się przez analizę wydruków lub migających lampek na panelach. Teraz mamy luksus wypisywania informacji na konsolę w czasie rzeczywistym. Istnieją również zaawansowane narzędzia jak loggery (np. `Log4j`, `SLF4J`) pozwalające kontrolować poziomy logowania i kierunki wyjściowe.

Zamiast drukowania bezpośrednio na konsolę, debugowanie można przeprowadzić używając wbudowanych debuggerów w IDE takich jak IntelliJ IDEA czy Eclipse, które oferują bardziej elastyczne możliwości. Pozwalają na ustawianie breakpointów, krokowe przeglądanie kodu oraz inspekcję zmiennych w czasie wykonania programu.

Implementacja drukowania w Javie opiera się na klasach `PrintStream` i `PrintWriter`. Oferują one różne metody do formatowania i wyjścia danych. Dobrą praktyką jest minimalizowanie debugowania przez wypisywanie i korzystanie z "prawdziwego" debugowania lub logowania w celu utrzymania czytelności kodu.

## See Also (Zobacz także):
- [Oracle Java Documentation on PrintStream](https://docs.oracle.com/javase/8/docs/api/java/io/PrintStream.html)
- [SLF4J project page](http://www.slf4j.org/)
- [Jak ustawiać breakpointy w IntelliJ IDEA](https://www.jetbrains.com/help/idea/using-breakpoints.html)
