---
title:                "Schrijven naar standaardfout"
aliases:
- /nl/kotlin/writing-to-standard-error.md
date:                  2024-01-28T22:13:26.312264-07:00
model:                 gpt-4-0125-preview
simple_title:         "Schrijven naar standaardfout"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/kotlin/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Standaardfout (stderr) is een stroom waar een programma zijn foutberichten naar schrijft. Programmeurs gebruiken het om foutlogboeken te scheiden van standaarduitvoer (stdout) om efficiënter te debuggen en logboekregistratie te stroomlijnen.

## Hoe te:

Hier is een eenvoudig Kotlin-fragment om naar standaardfout te printen:

```kotlin
fun main() {
    System.err.println("Oeps, er is een fout opgetreden.")
}
```

En de uitvoer in je console ziet er zo uit (stijl kan variëren per terminal):

```
Oeps, er is een fout opgetreden.
```

## Diepere duik

Oorspronkelijk in Unix-achtige systemen is de redenering voor stderr duidelijk: stderr maakt het mogelijk om foutberichten naar het scherm of een ander bestand dan de normale uitvoer te sturen. Het helpt bij het onderscheiden van normale data van foutberichten, vooral nuttig wanneer de uitvoer elders wordt doorgestuurd.

Alternatieven voor `System.err.println` zijn onder andere het gebruik van een logboekregistratieframework zoals Logback of log4j, die meer controle en opties bieden, zoals logniveaus en bestandsuitvoer.

De `System.err` in Kotlin is geërfd van Java's `System`-klasse, vergelijkbaar met `System.out` voor standaarduitvoer, beide zijn PrintStream-objecten. Standaard print `System.err` naar de console. Het kan echter worden omgeleid om naar een bestand of een andere uitvoerstroom te schrijven.

## Zie ook

- De Kotlin-documentatie over basisinvoer/uitvoer: https://kotlinlang.org/docs/basic-io.html
- Informatie over Unix standaardstromen: https://nl.wikipedia.org/wiki/Standaardstromen
- Logback, een populair logboekregistratieframework: http://logback.qos.ch/
- Apache log4j, een ander logboekregistratieframework: https://logging.apache.org/log4j/2.x/
