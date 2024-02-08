---
title:                "Skriva till standardfel"
aliases:
- sv/java/writing-to-standard-error.md
date:                  2024-02-03T19:33:36.684509-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skriva till standardfel"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva till standardfel (stderr) innebär att skicka felmeddelanden och diagnostik till konsolen eller terminalen. Programmerare gör detta för att separera felinformation från standardutdata (stdout), vilket underlättar felsökning och analys av loggar.

## Hur man gör:

### Grundläggande stderr-utdata i Java
Java erbjuder ett enkelt sätt att skriva till stderr med `System.err.print()` eller `System.err.println()`. Så här gör du:

```java
public class StdErrExample {
    public static void main(String[] args) {
        try {
            int division = 10 / 0;
        } catch (ArithmeticException e) {
            System.err.println("Fel: Kan inte dela med noll.");
        }
    }
}
```

Exempelutdata:

```
Fel: Kan inte dela med noll.
```

Detta kommer direkt att skriva ut felmeddelandet till stderr-strömmen.

### Använda en Loggare för Avancerad Felhantering
För applikationer som behöver mer sofistikerad felhantering och loggning är det vanligt att använda ett loggningsbibliotek som SLF4J med Logback eller Log4J2. Detta möjliggör större flexibilitet i hanteringen av felutdata, inklusive omdirigering till filer, filtrering och formatering.

#### Exempel med Logback

Lägg först till beroendet för Logback i din `pom.xml` (Maven) eller `build.gradle` (Gradle) fil. För Maven:

```xml
<dependency>
    <groupId>ch.qos.logback</groupId>
    <artifactId>logback-classic</artifactId>
    <version>1.2.3</version>
</dependency>
```

Sedan kan du använda följande kod för att logga fel:

```java
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LoggerExample {
    private static final Logger logger = LoggerFactory.getLogger(LoggerExample.class);
    
    public static void main(String[] args) {
        try {
            int resultat = 10 / 0;
        } catch (ArithmeticException e) {
            logger.error("Fel: Kan inte dela med noll.", e);
        }
    }
}
```

Detta kommer att skriva ut felmeddelandet tillsammans med en stacktrace till konsolen eller en fil, beroende på Logbacks konfiguration.

Att använda loggningsramverk som Logback ger mer kontroll över felhantering, vilket underlättar hantering av stora applikationer och system.
