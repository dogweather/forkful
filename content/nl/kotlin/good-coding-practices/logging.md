---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:16.225747-07:00
description: "Loggen, in de kern, is de praktijk van het vastleggen van gebeurtenissen\
  \ en gegevens van een softwareapplicatie naar een externe uitvoer, zoals een\u2026"
lastmod: '2024-03-13T22:44:50.774324-06:00'
model: gpt-4-0125-preview
summary: "Loggen, in de kern, is de praktijk van het vastleggen van gebeurtenissen\
  \ en gegevens van een softwareapplicatie naar een externe uitvoer, zoals een\u2026"
title: Logboekregistratie
weight: 17
---

## Wat & Waarom?

Loggen, in de kern, is de praktijk van het vastleggen van gebeurtenissen en gegevens van een softwareapplicatie naar een externe uitvoer, zoals een bestand of console. Programmeurs loggen dingen om door code heen te traceren, problemen op te lossen en een oogje in het zeil te houden op het gedrag van een app in het wild, wat cruciale inzichten biedt die op geen andere manier zo effectief verkregen kunnen worden.

## Hoe te:

In Kotlin kan loggen worden gedaan met behulp van de ingebouwde `println()` functie voor eenvoudige gevallen, of met meer geavanceerde bibliotheken zoals SLF4J met Logback of Log4j voor geavanceerde behoeften.

Hieronder staat een basisvoorbeeld met `println()`:

```Kotlin
fun main() {
    println("Eenvoudig logbericht: Applicatie gestart.")
    // ... hier enige applicatielogica ...
    try {
        // Simuleer een fout
        throw Exception("Gesimuleerde fout")
    } catch (e: Exception) {
        println("Fout logbericht: " + e.message)
    }
}
```

Uitvoer:
```
Eenvoudig logbericht: Applicatie gestart.
Fout logbericht: Gesimuleerde fout
```

En hier is een fragment met SLF4J en Logback geconfigureerd:

```Kotlin
import org.slf4j.LoggerFactory

private val logger = LoggerFactory.getLogger("MyAppLogger")

fun main() {
    logger.info("Gestructureerd logbericht: App gelanceerd.")
    // ... hier enige applicatielogica ...
    try {
        // Simuleer een fout
        throw Exception("Gesimuleerde fout")
    } catch (e: Exception) {
        logger.error("Gestructureerd foutlog: ", e)
    }
}
```

Aannemende de juiste Logback-configuratie, de uitvoer zou geformatteerd zijn en zou er ongeveer zo uitzien wanneer die naar een logbestand wordt geschreven:
```
[INFO] - 2023-03-29 14:15:42 - MyAppLogger - Gestructureerd logbericht: App gelanceerd.
[ERROR] - 2023-03-29 14:15:43 - MyAppLogger - Gestructureerd foutlog: 
java.lang.Exception: Gesimuleerde fout
   at com.myapp.Main.main(Main.kt:10)
```

## Diepgaand

Historisch heeft het loggen in software zich ontwikkeld met de toenemende complexiteit van applicaties en systemen. Eenvoudige printverklaringen waren voldoende in de beginjaren, waar programma's vaak werden uitgevoerd en gedebugd door de ontwikkelaar zelf. Maar naarmate systemen verbonden werden en in verschillende omgevingen over verschillende gebruikers liepen, werd een robuust en blijvend logsysteem cruciaal.

Voordat Kotlin populair werd, adopteerden Java-ontwikkelaars op grote schaal bibliotheken zoals Log4j en later SLF4J. Deze hebben vergelijkbare praktijken in Kotlin geïnspireerd, waarbij gebruik wordt gemaakt van de interoperabiliteit van Kotlin met Java-bibliotheken. SLF4J werkt als een abstractielaag, waardoor de werkelijke logimplementatie vervangen kan worden—meestal zijn Logback of Log4j2 de voorkeurskeuzes.

Kotlin maakt ook multi-platform logoplossingen mogelijk die werken op JVM, JavaScript en Native, bijvoorbeeld via het `expect`/`actual` mechanisme, dat de platformspecifieke implementaties abstraheert.

In tegenstelling tot toegewijde logbibliotheken, blijft println als de eenvoudigste vorm van loggen bestaan omdat het geen extra opzet of afhankelijkheden vereist; het is echter meestal ongeschikt voor productietoepassingen vanwege het gebrek aan functies zoals logniveaus, logrotatie en gestructureerde formaten.

Andere gemeenschappelijke functies van geavanceerde logframeworks zijn:

- Logniveaus (DEBUG, INFO, WARN, ERROR, etc.) om de urgentie van logberichten te categoriseren.
- Uitvoer naar verschillende eindpunten, zoals console, bestand, databases of netwerkdiensten.
- Automatische logrotatie en retentiebeleid.
- Ondersteuning voor gedistribueerde tracing voor microservices architectuur.
- Gestructureerd loggen met formaten zoals JSON, wat goed integreert met loganalysesystemen.

Deze tools en functies zijn cruciaal voor het onderhouden van een betrouwbaar, waarneembaar systeem, met name in complexe, gedistribueerde of sterk geschaalde omgevingen.

## Zie ook

Voor verdere studie en inzicht in Kotlin loggen, bekijk:

- SLF4J (Simple Logging Facade for Java) [http://www.slf4j.org/](http://www.slf4j.org/)
- Logback, de opvolger van Log4j [http://logback.qos.ch/](http://logback.qos.ch/)
- Log4j 2 [https://logging.apache.org/log4j/2.x/](https://logging.apache.org/log4j/2.x/)
- Kotlin Multiplatform documentatie over 'expect' en 'actual' verklaringen: [https://kotlinlang.org/docs/multiplatform.html](https://kotlinlang.org/docs/multiplatform.html)
- Een gids voor gestructureerd loggen in Kotlin: [https://ktor.io/docs/logging.html](https://ktor.io/docs/logging.html)
