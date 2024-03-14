---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:52.941240-07:00
description: "Loggen is in essentie het proces van het vastleggen van gebeurtenissen\
  \ die plaatsvinden binnen een softwareapplicatie. Programmeurs loggen deze\u2026"
lastmod: '2024-03-13T22:44:50.688824-06:00'
model: gpt-4-0125-preview
summary: "Loggen is in essentie het proces van het vastleggen van gebeurtenissen die\
  \ plaatsvinden binnen een softwareapplicatie. Programmeurs loggen deze\u2026"
title: Logboekregistratie
---

{{< edit_this_page >}}

## Wat & Waarom?
Loggen is in essentie het proces van het vastleggen van gebeurtenissen die plaatsvinden binnen een softwareapplicatie. Programmeurs loggen deze gebeurtenissen om runtime-informatie te verzamelen, problemen te debuggen, systeemgedrag te monitoren en een audit trail te creëren voor beveiligings- en nalevingsdoeleinden.

## Hoe:
Hier is een eenvoudige manier om te beginnen met loggen in Java met behulp van het ingebouwde `java.util.logging` pakket.

```java
import java.util.logging.Logger;
import java.util.logging.Level;

public class AppLogging {
    private final static Logger LOGGER = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

    public static void main(String[] args) {
        LOGGER.info("Logging een INFO-niveau bericht");

        try {
            int deling = 10 / 0;
        } catch (ArithmeticException e) {
            LOGGER.log(Level.SEVERE, "Uitzondering voorgevallen", e);
        }
    }
}
```

Dit zou een uitvoer produceren die ongeveer zo luidt:

```
Jul 03, 2023 2:00:00 PM AppLogging main
INFO: Logging een INFO-niveau bericht
Jul 03, 2023 2:00:00 PM AppLogging main
SEVERE: Uitzondering voorgevallen
java.lang.ArithmeticException: / door nul
    bij AppLogging.main(AppLogging.java:10)
```

## Diepere Duik
Loggen in Java is behoorlijk geëvolueerd. Historisch gezien was loggen meer ad-hoc met systeemuitvoer en zelfgeschreven mechanismen. Echter, de behoefte aan standaardisatie leidde tot log-API's zoals `Log4j` en `SLF4J`. Het `java.util.logging` pakket zelf werd geïntroduceerd in JDK 1.4, wat een gestandaardiseerde manier bood om berichten te loggen.

Alternatieven voor `java.util.logging` (JUL) zijn onder andere Log4j 2 en SLF4J. Hoewel JUL ingebouwd is in Java en dus geen extra afhankelijkheden vereist, bieden zowel Log4j 2 als SLF4J geavanceerdere functies zoals meer gedetailleerde controle over de logconfiguratie, asynchroon loggen en betere prestaties.

Wat betreft implementatie kan loggen zowel synchroon zijn, waarbij elk logbericht wordt verwerkt in de draad die het heeft gegenereerd, of asynchroon, waarbij berichten worden overgedragen aan een aparte draad. Asynchroon loggen kan de prestaties verbeteren, maar introduceert complexiteit aangezien men gelijktijdigheid moet afhandelen en ervoor zorgen dat logberichten niet verloren gaan bij applicatiecrashes.

## Zie Ook
- [Log4j 2](https://logging.apache.org/log4j/2.x/)
- [SLF4J](http://www.slf4j.org/)
- [Oracle's officiële overzicht van loggen](https://docs.oracle.com/javase/8/docs/technotes/guides/logging/overview.html)
- [Tutorial over java.util.logging](https://www.vogella.com/tutorials/Logging/article.html)
