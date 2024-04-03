---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:46.343652-07:00
description: "Wie: In Kotlin kann das Schreiben auf stderr mit `System.err.println()`\
  \ erreicht werden. Diese Methode \xE4hnelt `System.out.println()`, leitet die Ausgabe\u2026"
lastmod: '2024-03-13T22:44:53.862480-06:00'
model: gpt-4-0125-preview
summary: In Kotlin kann das Schreiben auf stderr mit `System.err.println()` erreicht
  werden.
title: Schreiben auf Standardfehler
weight: 25
---

## Wie:
In Kotlin kann das Schreiben auf stderr mit `System.err.println()` erreicht werden. Diese Methode ähnelt `System.out.println()`, leitet die Ausgabe jedoch an den Standardfehlerstrom statt an den Standardausgabestrom.

```kotlin
fun main() {
    System.err.println("Dies ist eine Fehlermeldung!")
}
```

Beispielausgabe:
```
Dies ist eine Fehlermeldung!
```

Für strukturiertere oder komplexere Anwendungen, insbesondere solche, die Protokollierungsframeworks wie Logback oder SLF4J verwenden, können Sie Logger konfigurieren, um für bestimmte Protokollebenen (z. B. ERROR) auf stderr zu schreiben.

Verwendung von SLF4J mit Logback:

1. Fügen Sie zunächst die SLF4J-API und die Logback-Implementierung zu Ihrem `build.gradle` hinzu:

```groovy
dependencies {
    implementation 'org.slf4j:slf4j-api:1.7.30'
    implementation 'ch.qos.logback:logback-classic:1.2.3'
}
```

2. Konfigurieren Sie anschließend Logback (in `src/main/resources/logback.xml`), um Fehlermeldungen auf stderr umzuleiten:

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

3. Verwenden Sie dann SLF4J in Ihrem Kotlin-Code, um Fehlermeldungen zu protokollieren:

```kotlin
import org.slf4j.LoggerFactory

fun main() {
    val logger = LoggerFactory.getLogger("ExampleLogger")
    logger.error("Dies ist eine Fehlerprotokollmeldung!")
}
```

Beispielausgabe (auf stderr):
```
2023-04-01 12:34:56 [main] ERROR ExampleLogger - Dies ist eine Fehlerprotokollmeldung!
```
