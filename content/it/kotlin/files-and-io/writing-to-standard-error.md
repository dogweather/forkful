---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:44.748694-07:00
description: "Come fare: In Kotlin, scrivere su stderr pu\xF2 essere realizzato utilizzando\
  \ `System.err.println()`. Questo metodo \xE8 simile a `System.out.println()`, ma\u2026"
lastmod: '2024-03-13T22:44:43.407071-06:00'
model: gpt-4-0125-preview
summary: "In Kotlin, scrivere su stderr pu\xF2 essere realizzato utilizzando `System.err.println()`."
title: Scrivere sull'errore standard
weight: 25
---

## Come fare:
In Kotlin, scrivere su stderr può essere realizzato utilizzando `System.err.println()`. Questo metodo è simile a `System.out.println()`, ma indirizza l'output al flusso di errore standard piuttosto che al flusso di output standard.

```kotlin
fun main() {
    System.err.println("Questo è un messaggio di errore!")
}
```

Output di esempio:
```
Questo è un messaggio di errore!
```

Per applicazioni più strutturate o complesse, in particolare quelle che coinvolgono framework di logging come Logback o SLF4J, è possibile configurare logger per scrivere su stderr per determinati livelli di log (ad esempio, ERROR).

Utilizzando SLF4J con Logback:

1. Prima, aggiungi l'API SLF4J e l'implementazione Logback nel tuo `build.gradle`:

```groovy
dependencies {
    implementation 'org.slf4j:slf4j-api:1.7.30'
    implementation 'ch.qos.logback:logback-classic:1.2.3'
}
```

2. Successivamente, configura Logback (in `src/main/resources/logback.xml`) per indirizzare i messaggi di livello errore su stderr:

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

3. Quindi, usa SLF4J nel tuo codice Kotlin per registrare i messaggi di errore:

```kotlin
import org.slf4j.LoggerFactory

fun main() {
    val logger = LoggerFactory.getLogger("ExampleLogger")
    logger.error("Questo è un messaggio di log di errore!")
}
```

Output di esempio (su stderr):
```
2023-04-01 12:34:56 [main] ERROR ExampleLogger - Questo è un messaggio di log di errore!
```
