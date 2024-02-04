---
title:                "Pisanie do standardowego błędu"
date:                  2024-02-03T19:33:52.084964-07:00
model:                 gpt-4-0125-preview
simple_title:         "Pisanie do standardowego błędu"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Zapis do standardowego błędu (stderr) polega na wyprowadzaniu komunikatów o błędach i diagnostyki na konsolę lub terminal. Programiści robią to, aby oddzielić informacje o błędach od standardowego wyjścia (stdout), ułatwiając debugowanie i analizę logów.

## Jak to zrobić:

### Podstawowy zapis do stderr w Javie
Java oferuje prosty sposób na zapis do stderr za pomocą `System.err.print()` lub `System.err.println()`. Oto jak to zrobisz:

```java
public class StdErrExample {
    public static void main(String[] args) {
        try {
            int division = 10 / 0;
        } catch (ArithmeticException e) {
            System.err.println("Błąd: Nie można dzielić przez zero.");
        }
    }
}
```

Przykładowe wyjście:

```
Błąd: Nie można dzielić przez zero.
```

To bezpośrednio wydrukuje komunikat o błędzie do strumienia standardowego błędu.

### Użycie Loggera dla Zaawansowanego Obsługiwania Błędów
W aplikacjach wymagających bardziej zaawansowanego obsługiwania błędów i logowania, powszechne jest używanie biblioteki logowania jak SLF4J z Logbackiem lub Log4J2. Pozwala to na większą elastyczność w zarządzaniu wyjściem błędu, w tym przekierowanie do pliku, filtrowanie i formatowanie.

#### Przykład z Logbackiem

Najpierw, dodaj zależność do Logbacka do pliku `pom.xml` (Maven) lub `build.gradle` (Gradle). Dla Mavena:

```xml
<dependency>
    <groupId>ch.qos.logback</groupId>
    <artifactId>logback-classic</artifactId>
    <version>1.2.3</version>
</dependency>
```

Następnie możesz użyć poniższego kodu do logowania błędów:

```java
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LoggerExample {
    private static final Logger logger = LoggerFactory.getLogger(LoggerExample.class);
    
    public static void main(String[] args) {
        try {
            int result = 10 / 0;
        } catch (ArithmeticException e) {
            logger.error("Błąd: Nie można dzielić przez zero.", e);
        }
    }
}
```

To wydrukuje komunikat o błędzie wraz ze śladem stosu na konsolę lub do pliku, w zależności od konfiguracji Logbacka.

Używanie frameworków logowania jak Logback daje większą kontrolę nad obsługą błędów, ułatwiając zarządzanie dużymi aplikacjami i systemami.
