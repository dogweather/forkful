---
title:                "Loggføring"
aliases:
- no/java/logging.md
date:                  2024-01-26T01:06:49.799020-07:00
model:                 gpt-4-1106-preview
simple_title:         "Loggføring"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/logging.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Logging er i bunn og grunn prosessen med å ta opp hendelser som inntreffer i en programvareapplikasjon. Programmerere logger disse hendelsene for å fange opp kjøretidsinformasjon, feilsøke problemer, overvåke systemoppførsel og skape en sporingslogg for sikkerhet og etterlevelse av regler.

## Hvordan:
Her er en enkel måte å komme i gang med logging i Java ved å bruke den innebygde pakken `java.util.logging`.

```java
import java.util.logging.Logger;
import java.util.logging.Level;

public class AppLogging {
    private final static Logger LOGGER = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

    public static void main(String[] args) {
        LOGGER.info("Logger en INFO-nivå melding");

        try {
            int division = 10 / 0;
        } catch (ArithmeticException e) {
            LOGGER.log(Level.SEVERE, "Unntak oppstod", e);
        }
    }
}
```

Dette vil produsere en utskrift i denne stilen:

```
Jul 03, 2023 2:00:00 PM AppLogging main
INFO: Logger en INFO-nivå melding
Jul 03, 2023 2:00:00 PM AppLogging main
SEVERE: Unntak oppstod
java.lang.ArithmeticException: / by zero
    at AppLogging.main(AppLogging.java:10)
```

## Dypdykk
Logging i Java har utviklet seg en god del. Historisk sett var logging mer tilfeldig med systemutskrifter og selvskrevne mekanismer. Behovet for standardisering førte til logging-API-er som `Log4j` og `SLF4J`. Pakken `java.util.logging` ble introdusert i JDK 1.4 og tilbyr en standardisert måte å logge meldinger på.

Alternativer til `java.util.logging` (JUL) inkluderer Log4j 2 og SLF4J. Mens JUL er innebygd i Java og dermed ikke krever ekstra avhengigheter, tilbyr både Log4j 2 og SLF4J mer avanserte funksjoner som mer granulær kontroll over logg-konfigurasjon, asynkron logging og bedre ytelse.

Når det kommer til implementering, kan logging være enten synkron, hvor hver loggmelding behandles i tråden som genererte den, eller asynkron, hvor meldinger overleveres til en separat tråd. Asynkron logging kan forbedre ytelse, men introduserer kompleksitet siden man må håndtere samtidighet og sikre at loggmeldinger ikke går tapt ved applikasjonskrasj.

## Se Også
- [Log4j 2](https://logging.apache.org/log4j/2.x/)
- [SLF4J](http://www.slf4j.org/)
- [Oracles offisielle oversikt over logging](https://docs.oracle.com/javase/8/docs/technotes/guides/logging/overview.html)
- [Veiledning på java.util.logging](https://www.vogella.com/tutorials/Logging/article.html)
