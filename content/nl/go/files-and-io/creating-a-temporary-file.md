---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:55:27.294574-07:00
description: "Hoe te: In Go bood het `ioutil`-pakket oorspronkelijk hulpprogramma's\
  \ voor het aanmaken van tijdelijke bestanden. Echter, Go 1.16 bevorderde het gebruik\u2026"
lastmod: '2024-03-13T22:44:50.307296-06:00'
model: gpt-4-0125-preview
summary: In Go bood het `ioutil`-pakket oorspronkelijk hulpprogramma's voor het aanmaken
  van tijdelijke bestanden.
title: Een tijdelijk bestand aanmaken
weight: 21
---

## Hoe te:
In Go bood het `ioutil`-pakket oorspronkelijk hulpprogramma's voor het aanmaken van tijdelijke bestanden. Echter, Go 1.16 bevorderde het gebruik van de functies van het `os` en `io/ioutil` pakket naar meer georganiseerde plekken. Nu worden de `os`- en `io`-pakketten de voorkeur gegeven voor het omgaan met tijdelijke bestanden.

Hier is een stap-voor-stap handleiding voor het creëren, schrijven naar, en verwijderen van een tijdelijk bestand:

1. **Een tijdelijk bestand aanmaken:**

Met de `os.CreateTemp`-functie kunt u een tijdelijk bestand aanmaken. Zonder een directory op te geven, gebruikt het de standaard temp map van uw besturingssysteem.

```go
package main

import (
    "io/ioutil"
    "log"
    "os"
)

func main() {
    tmpFile, err := ioutil.TempFile("", "example.*.txt")
    if err != nil {
        log.Fatal(err)
    }
    log.Printf("Tijdelijk bestand aangemaakt: %s\n", tmpFile.Name())

    defer os.Remove(tmpFile.Name()) // Opruimen
}
```

2. **Schrijven naar het tijdelijke bestand:**

Schrijven naar het bestand kan worden bereikt met de `Write`-methode of andere schrijffuncties van de `io`- of `bufio`-pakketten.

```go
_, err = tmpFile.Write([]byte("Hallo, Wereld!"))
if err != nil {
    log.Fatal(err)
}
```

3. **Lezen van het tijdelijke bestand:**

Lezen volgt op een soortgelijke manier, door gebruik te maken van de `Read`-methode van het bestand, of door gebruik te maken van hulpprogramma's uit de `io`- of `bufio`-pakketten.

```go
data, err := ioutil.ReadFile(tmpFile.Name())
if err != nil {
    log.Fatal(err)
}
log.Printf("Gegevens gelezen: %s\n", string(data))
```

4. **Het tijdelijke bestand verwijderen:**

Terwijl de `defer os.Remove(tmpFile.Name())`-verklaring in de aanmaakfase ervoor zorgt dat het tijdelijke bestand wordt verwijderd na het beëindigen van het programma, kan expliciete verwijdering naar behoefte worden beheerd.

Voorbeelduitvoer:
```
2023/04/01 15:00:00 Tijdelijk bestand aangemaakt: /tmp/example.123456.txt
2023/04/01 15:00:00 Gegevens gelezen: Hallo, Wereld!
```

## Diepgaande duik
Het mechanisme achter de omgang van Go met tijdelijke bestanden is geëvolueerd. Aanvankelijk werd het aanmaken van tijdelijke bestanden vooral beheerd door de nu afgeschafte `ioutil.TempFile`-functie, dit weerspiegelt bredere trends in softwareontwikkeling richting veiligere en efficiëntere bestandshandeling praktijken. De stap om deze functionaliteiten te integreren in de `os`- en `io`-pakketten met Go 1.16 signaleert een bredere push naar het stroomlijnen van de standaardbibliotheek van de taal en het aanmoedigen van het gebruik van meer verenigde en samenhangende API's.

Hoewel het gebruik van tijdelijke bestanden een veelvoorkomende en vaak essentiële praktijk is in programmeren, is het belangrijk op te merken dat te zwaar vertrouwen op hen voor het opslaan van grote hoeveelheden gegevens of voor langetermijntaken kan leiden tot prestatieproblemen. Bovendien, wanneer het aanmaken van tijdelijke bestanden niet strak wordt gecontroleerd of wanneer ze niet adequaat worden opgeruimd, kan dit leiden tot resource-lekken die negatief kunnen uitwerken op het bestandssysteem. In scenario's die permanente opslag vereisen of het omgaan met substantiële gegevensstromen behoeven, bieden alternatieven zoals databases of in-memory datastores vaak betere prestaties en betrouwbaarheid in vergelijking met tijdelijke bestanden.
