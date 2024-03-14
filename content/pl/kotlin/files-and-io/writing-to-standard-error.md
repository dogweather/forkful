---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:59.855425-07:00
description: "Wypisywanie do standardowego b\u0142\u0119du (stderr) dotyczy przekazywania\
  \ komunikat\xF3w o b\u0142\u0119dach i diagnostyki do oddzielnego strumienia, odr\u0119\
  bnego od\u2026"
lastmod: '2024-03-13T22:44:35.381582-06:00'
model: gpt-4-0125-preview
summary: "Wypisywanie do standardowego b\u0142\u0119du (stderr) dotyczy przekazywania\
  \ komunikat\xF3w o b\u0142\u0119dach i diagnostyki do oddzielnego strumienia, odr\u0119\
  bnego od\u2026"
title: "Pisanie do standardowego b\u0142\u0119du"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Wypisywanie do standardowego błędu (stderr) dotyczy przekazywania komunikatów o błędach i diagnostyki do oddzielnego strumienia, odrębnego od standardowego wyjścia (stdout), co pozwala na lepsze zarządzanie błędami i parsowanie logów. Programiści stosują tę praktykę, aby ułatwić debugowanie oraz zapewnić, że komunikaty o błędach można łatwo zidentyfikować i przekierować w razie potrzeby, utrzymując czystość logów wyjściowych czy komunikatów użytkownika.

## Jak to zrobić:

W Kotlinie, zapis do stderr można osiągnąć za pomocą `System.err.println()`. Ta metoda jest podobna do `System.out.println()`, ale kieruje wyjście do strumienia błędu standardowego, a nie do standardowego strumienia wyjściowego.

```kotlin
fun main() {
    System.err.println("To jest komunikat o błędzie!")
}
```

Przykładowe wyjście:
```
To jest komunikat o błędzie!
```

W przypadku bardziej złożonych lub strukturalnych aplikacji, szczególnie tych wykorzystujących frameworki logowania takie jak Logback czy SLF4J, można skonfigurować loggery do zapisywania do stderr dla określonych poziomów logowania (np. ERROR).

Używanie SLF4J z Logback:

1. Najpierw, dodaj API SLF4J i implementację Logback do pliku `build.gradle`:

```groovy
dependencies {
    implementation 'org.slf4j:slf4j-api:1.7.30'
    implementation 'ch.qos.logback:logback-classic:1.2.3'
}
```

2. Następnie, skonfiguruj Logback (w `src/main/resources/logback.xml`) aby kierować komunikaty na poziomie błędu do stderr:

```xml
<configuration>
    <appender name="STDERR" class="ch.qos.logback.core.ConsoleAppender">
        <target>System.err</target>
        <encoder>
            <pattern>%d{yyyy-MM-dd HH:mm:ss} [%thread] %-5level %logger{36} - %msg%n</pattern>
        </encoder>
    </appender>
    
    <root level="error">
        <appender-ref ref="STDERR" />
    </root>
</configuration>
```

3. Następnie, użyj SLF4J w swoim kodzie Kotlin do logowania komunikatów o błędach:

```kotlin
import org.slf4j.LoggerFactory

fun main() {
    val logger = LoggerFactory.getLogger("ExampleLogger")
    logger.error("To jest komunikat logowania błędu!")
}
```

Przykładowe wyjście (do stderr):
```
2023-04-01 12:34:56 [main] ERROR ExampleLogger - To jest komunikat logowania błędu!
```
