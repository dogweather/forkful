---
title:                "Skriving til standardfeil"
date:                  2024-02-03T19:34:11.061457-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skriving til standardfeil"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å skrive til standard feil (stderr) handler om å sende ut feilmeldinger og diagnostikk til en separat strøm, forskjellig fra standard utdata (stdout), som gjør det mulig for bedre feilhåndtering og loggtolkning. Programmerere gjør dette for å lette feilsøking og for å sikre at feilmeldinger kan lett identifiseres og omdirigeres ved behov, og opprettholder rene utdata logger eller brukermeldinger.

## Hvordan:

I Kotlin kan skriving til stderr oppnås ved å bruke `System.err.println()`. Denne metoden er lik `System.out.println()` men dirigerer utdata til standard feilstrøm i stedet for standard utdatastrøm.

```kotlin
fun main() {
    System.err.println("Dette er en feilmelding!")
}
```

Eksempel på utdata:
```
Dette er en feilmelding!
```

For mer strukturerte eller komplekse applikasjoner, spesielt de som involverer loggføringsrammeverk som Logback eller SLF4J, kan du konfigurere loggere for å skrive til stderr for visse loggnivåer (f.eks., ERROR).

Bruk av SLF4J med Logback:

1. Først, legg til SLF4J API og Logback-implementasjon i din `build.gradle`:

```groovy
dependencies {
    implementation 'org.slf4j:slf4j-api:1.7.30'
    implementation 'ch.qos.logback:logback-classic:1.2.3'
}
```

2. Deretter, konfigurer Logback (i `src/main/resources/logback.xml`) til å dirigere feilnivåmeldinger til stderr:

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

3. Deretter, bruk SLF4J i din Kotlin kode for å logge feilmeldinger:

```kotlin
import org.slf4j.LoggerFactory

fun main() {
    val logger = LoggerFactory.getLogger("ExampleLogger")
    logger.error("Dette er en feilloggmelding!")
}
```

Eksempel på utdata (til stderr):
```
2023-04-01 12:34:56 [main] ERROR ExampleLogger - Dette er en feilloggmelding!
```
