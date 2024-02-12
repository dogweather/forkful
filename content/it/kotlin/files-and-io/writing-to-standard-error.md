---
title:                "Scrivere sull'errore standard"
aliases:
- it/kotlin/writing-to-standard-error.md
date:                  2024-02-03T19:33:44.748694-07:00
model:                 gpt-4-0125-preview
simple_title:         "Scrivere sull'errore standard"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e Perché?

Scrivere su standard error (stderr) consiste nell'inviare messaggi di errore e diagnostica su un flusso separato, distinto dall'output standard (stdout), ciò consente una migliore gestione degli errori e analisi dei log. I programmatori fanno ciò per facilitare il debug e per garantire che i messaggi di errore possano essere facilmente identificati e reindirizzati se necessario, mantenendo puliti i log di output o i messaggi agli utenti.

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
