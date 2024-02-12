---
title:                "Skriva till standardfel"
aliases:
- sv/kotlin/writing-to-standard-error.md
date:                  2024-02-03T19:34:00.488331-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skriva till standardfel"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva till standardfel (stderr) handlar om att skicka ut felmeddelanden och diagnostik till en separat ström, skild från standardutdata (stdout), vilket möjliggör bättre felsökning och loggtolkning. Programmerare gör detta för att underlätta felsökning och för att säkerställa att felmeddelanden enkelt kan identifieras och omdirigeras vid behov, vilket bibehåller rena utdata loggar eller användarmeddelanden.

## Hur man gör:

I Kotlin kan skrivning till stderr uppnås med `System.err.println()`. Denna metod är lik `System.out.println()` men dirigerar utdatan till standardfelströmmen istället för standardutdataströmmen.

```kotlin
fun main() {
    System.err.println("Detta är ett felmeddelande!")
}
```

Exempel på utdata:
```
Detta är ett felmeddelande!
```

För mer strukturerade eller komplexa applikationer, särskilt de som involverar loggningsramverk som Logback eller SLF4J, kan du konfigurera loggarna att skriva till stderr för vissa loggnivåer (t.ex. ERROR).

Använda SLF4J med Logback:

1. Först, lägg till SLF4J API:t och Logback-implementationen i din `build.gradle`:

```groovy
dependencies {
    implementation 'org.slf4j:slf4j-api:1.7.30'
    implementation 'ch.qos.logback:logback-classic:1.2.3'
}
```

2. Nästa, konfigurera Logback (i `src/main/resources/logback.xml`) för att styra meddelanden på fel-nivå till stderr:

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

3. Använd sedan SLF4J i din Kotlin-kod för att logga felmeddelanden:

```kotlin
import org.slf4j.LoggerFactory

fun main() {
    val logger = LoggerFactory.getLogger("ExampleLogger")
    logger.error("Detta är ett felloggmeddelande!")
}
```

Exempel på utdata (till stderr):
```
2023-04-01 12:34:56 [main] ERROR ExampleLogger - Detta är ett felloggmeddelande!
```
