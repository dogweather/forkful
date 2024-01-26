---
title:                "Rejestrowanie zdarzeń"
date:                  2024-01-26T01:07:17.772198-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rejestrowanie zdarzeń"
programming_language: "Java"
category:             "Java"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/logging.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Logowanie to w zasadzie proces rejestrowania zdarzeń, które mają miejsce w aplikacji programowej. Programiści logują te zdarzenia, aby zbierać informacje o czasie wykonania, diagnozować problemy, monitorować zachowanie systemu oraz tworzyć ślad audytowy dla celów bezpieczeństwa i zgodności z przepisami.

## Jak to zrobić:
Oto prosty sposób na rozpoczęcie logowania w Javie przy użyciu wbudowanego pakietu `java.util.logging`.

```java
import java.util.logging.Logger;
import java.util.logging.Level;

public class AppLogging {
    private final static Logger LOGGER = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

    public static void main(String[] args) {
        LOGGER.info("Logowanie komunikatu na poziomie INFO");

        try {
            int division = 10 / 0;
        } catch (ArithmeticException e) {
            LOGGER.log(Level.SEVERE, "Wystąpił wyjątek", e);
        }
    }
}
```

To wygeneruje wynik w stylu:

```
Jul 03, 2023 2:00:00 PM AppLogging main
INFO: Logowanie komunikatu na poziomie INFO
Jul 03, 2023 2:00:00 PM AppLogging main
SEVERE: Wystąpił wyjątek
java.lang.ArithmeticException: / przez zero
    at AppLogging.main(AppLogging.java:10)
```

## Dogłębna analiza
Logowanie w Javie rozwinęło się dość znacznie. Historycznie, logowanie było bardziej ad-hoc z wykorzystaniem wyjść systemowych i własnoręcznie pisanych mechanizmów. Jednak potrzeba standaryzacji doprowadziła do powstania API do logowania, takich jak `Log4j` i `SLF4J`. Pakiet `java.util.logging` został wprowadzony w JDK 1.4, dostarczając ustandaryzowany sposób na rejestrowanie komunikatów.

Alternatywy dla `java.util.logging` (JUL) obejmują Log4j 2 i SLF4J. Chociaż JUL jest wbudowany w Javę i nie wymaga dodatkowych zależności, oba Log4j 2 i SLF4J oferują bardziej zaawansowane funkcje, takie jak bardziej szczegółowa kontrola konfiguracji logowania, asynchroniczne logowanie oraz lepsza wydajność.

Pod względem implementacji, logowanie może być synchroniczne, gdzie każdy komunikat logu jest przetwarzany w wątku, który go wygenerował, lub asynchroniczne, gdzie komunikaty są przekazywane do osobnego wątku. Asynchroniczne logowanie może poprawić wydajność, ale wprowadza złożoność, ponieważ należy radzić sobie ze współbieżnością i upewnić się, że komunikaty logów nie zostaną utracone przy awarii aplikacji.

## Zobacz także
- [Log4j 2](https://logging.apache.org/log4j/2.x/)
- [SLF4J](http://www.slf4j.org/)
- [Oficjalny przegląd logowania Oracle'a](https://docs.oracle.com/javase/8/docs/technotes/guides/logging/overview.html)
- [Samouczek dotyczący java.util.logging](https://www.vogella.com/tutorials/Logging/article.html)