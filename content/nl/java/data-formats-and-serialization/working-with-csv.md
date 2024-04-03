---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:21.488753-07:00
description: "Werken met CSV, wat staat voor Comma-Separated Values (komma-gescheiden\
  \ waarden), betekent het omgaan met gegevens in een eenvoudig tekstformaat waarbij\u2026"
lastmod: '2024-03-13T22:44:50.704836-06:00'
model: gpt-4-0125-preview
summary: Werken met CSV, wat staat voor Comma-Separated Values (komma-gescheiden waarden),
  betekent het omgaan met gegevens in een eenvoudig tekstformaat waarbij elke regel
  een gegevensrecord is met door komma's gescheiden velden.
title: Werken met CSV
weight: 37
---

## Wat & Waarom?

Werken met CSV, wat staat voor Comma-Separated Values (komma-gescheiden waarden), betekent het omgaan met gegevens in een eenvoudig tekstformaat waarbij elke regel een gegevensrecord is met door komma's gescheiden velden. Programmeurs duiken in CSV's omdat ze eenvoudig te gebruiken zijn, breed ondersteund worden en perfect zijn voor het uitwisselen van data tussen verschillende applicaties.

## Hoe:

Laten we CSV-bestanden lezen en schrijven in Java met behulp van de veelgebruikte `OpenCSV` bibliotheek. Voeg eerst de afhankelijkheid toe aan je `pom.xml` als je Maven gebruikt.

```xml
<dependency>
    <groupId>com.opencsv</groupId>
    <artifactId>opencsv</artifactId>
    <version>5.6</version> <!-- Controleer op de nieuwste versie -->
</dependency>
```

### Een CSV-bestand schrijven

```java
import com.opencsv.CSVWriter;
import java.io.FileWriter;
import java.io.IOException;

public class CSVWritingExample {
    public static void main(String[] args) {
        String[] kop = {"Naam", "Leeftijd", "Land"};
        String[] record1 = {"Alice", "24", "VS"};
        String[] record2 = {"Bob", "19", "Canada"};

        try (CSVWriter schrijver = new CSVWriter(new FileWriter("data.csv"))) {
            schrijver.writeNext(kop);
            schrijver.writeNext(record1);
            schrijver.writeNext(record2);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

### Een CSV-bestand lezen

```java
import com.opencsv.CSVReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.List;

public class CSVReadingExample {
    public static void main(String[] args) {

        try (CSVReader lezer = new CSVReader(new FileReader("data.csv"))) {
            List<String[]> r = lezer.readAll();
            r.forEach(x -> System.out.println(x[0] + ", " + x[1] + ", " + x[2]));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Voorbeelduitvoer na het lezen:

```
Naam, Leeftijd, Land
Alice, 24, VS
Bob, 19, Canada
```

## Diepgaande Duik

Historisch gezien worden CSV's al gebruikt sinds de vroege dagen van persoonlijke computing, waardoor ze een soort lingua franca zijn geworden voor gegevensuitwisseling. Alternatieven zoals JSON, XML of zelfs Excel-formaten kunnen geavanceerdere functies bieden, maar de eenvoud van CSV verzekert zijn voortbestaan. Wanneer je met Java werkt, terwijl `OpenCSV` een populaire keuze is, kun je ook `java.util.Scanner` of `java.io.BufferedReader` gebruiken voor zeer basistaken, hoewel je dan zelf de parsing moet afhandelen. `Apache Commons CSV` is een andere krachtige bibliotheek die beschikbaar is voor soortgelijke taken.

## Zie Ook

- De OpenCSV-homepage voor documentatie en handleidingen: http://opencsv.sourceforge.net/
- Apache Commons CSV voor een alternatieve aanpak: https://commons.apache.org/proper/commons-csv/
- Officiële Java-tutorials van Oracle voor I/O-operaties: https://docs.oracle.com/javase/tutorial/essential/io/
