---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:38.901649-07:00
description: "Hvordan: Java gir en grei m\xE5te \xE5 skrive til stderr p\xE5 ved bruk\
  \ av `System.err.print()` eller `System.err.println()`. Slik gj\xF8r du det."
lastmod: '2024-03-13T22:44:40.682810-06:00'
model: gpt-4-0125-preview
summary: "Java gir en grei m\xE5te \xE5 skrive til stderr p\xE5 ved bruk av `System.err.print()`\
  \ eller `System.err.println()`."
title: Skriving til standardfeil
weight: 25
---

## Hvordan:


### Grunnleggende stderr-utskrift i Java
Java gir en grei måte å skrive til stderr på ved bruk av `System.err.print()` eller `System.err.println()`. Slik gjør du det:

```java
public class StdErrExample {
    public static void main(String[] args) {
        try {
            int divisjon = 10 / 0;
        } catch (ArithmeticException e) {
            System.err.println("Feil: Kan ikke dele på null.");
        }
    }
}
```

Eksempel på utskrift:

```
Feil: Kan ikke dele på null.
```

Dette vil direkte skrive ut feilmeldingen til standard feilstrømmen.

### Bruk av en Logger for Avansert Feilhåndtering
For applikasjoner som trenger mer sofistikert feilhåndtering og logging, er det vanlig å bruke et loggingbibliotek som SLF4J med Logback eller Log4J2. Dette tillater mer fleksibilitet i håndteringen av feilutskrifter, inkludert filomdirigering, filtrering og formatering.

#### Eksempel med Logback
Først, legg til avhengigheten for Logback i din `pom.xml` (Maven) eller `build.gradle` (Gradle) fil. For Maven:

```xml
<dependency>
    <groupId>ch.qos.logback</groupId>
    <artifactId>logback-classic</artifactId>
    <version>1.2.3</version>
</dependency>
```

Deretter kan du bruke følgende kode for å logge feil:

```java
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LoggerExample {
    private static final Logger logger = LoggerFactory.getLogger(LoggerExample.class);
    
    public static void main(String[] args) {
        try {
            int resultat = 10 / 0;
        } catch (ArithmeticException e) {
            logger.error("Feil: Kan ikke dele på null.", e);
        }
    }
}
```

Dette vil skrive ut feilmeldingen sammen med en stakksporing til konsollen eller en fil, avhengig av Logback-konfigurasjonen.

Å bruke loggingsrammeverk som Logback gir mer kontroll over feilhåndtering, noe som gjør det enklere å håndtere store applikasjoner og systemer.
