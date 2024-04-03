---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:28.500154-07:00
description: "CSV-tiedostojen k\xE4sittelyyn kuuluu lukeminen ja kirjoittaminen pilkuilla\
  \ erotettuihin arvoihin (CSV) tiedostoihin, mik\xE4 on suosittu tiedonvaihdon muoto,\u2026"
lastmod: '2024-03-13T22:44:56.468113-06:00'
model: gpt-4-0125-preview
summary: "CSV-tiedostojen k\xE4sittelyyn kuuluu lukeminen ja kirjoittaminen pilkuilla\
  \ erotettuihin arvoihin (CSV) tiedostoihin, mik\xE4 on suosittu tiedonvaihdon muoto,\
  \ koska se on yksinkertainen ja laajalti tuettu."
title: "Ty\xF6skentely CSV:n kanssa"
weight: 37
---

## Kuinka:


### CSV-tiedoston lukeminen käyttäen standardia Java-kirjastoa
Java ei tue CSV:tä sisäänrakennetusti sen vakio-kirjastossa, mutta voit helposti lukea CSV-tiedoston käyttämällä `java.io` luokkia.

```java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class ReadCSVExample {
    public static void main(String[] args) {
        String rivi;
        String csvTiedosto = "data.csv"; // Määritä CSV-tiedoston polku
        try (BufferedReader br = new BufferedReader(new FileReader(csvTiedosto))) {
            while ((rivi = br.readLine()) != null) {
                String[] arvot = rivi.split(","); // Oletetaan, että pilkku on erotin
                // Käsittele dataa
                for (String arvo : arvot) {
                    System.out.print(arvo + " ");
                }
                System.out.println();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

### Kirjoittaminen CSV-tiedostoon käyttäen standardia Java-kirjastoa
Jotta voit kirjoittaa dataa CSV-tiedostoon, voit käyttää `java.io` luokkia, kuten `FileWriter` ja `BufferedWriter`.

```java
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class WriteCSVExample {
    public static void main(String[] args) {
        String[] data = {"John", "Doe", "30", "New York"};
        String csvTiedosto = "output.csv"; // Määritä ulostulon CSV-tiedoston polku

        try (BufferedWriter bw = new BufferedWriter(new FileWriter(csvTiedosto))) {
            StringBuilder sb = new StringBuilder();
            for (String arvo : data) {
                sb.append(arvo).append(","); // Oletetaan, että pilkku on erotin
            }
            sb.deleteCharAt(sb.length() - 1); // Poista viimeinen pilkku
            bw.write(sb.toString());
            bw.newLine();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

### Kolmannen osapuolen kirjaston käyttö: Apache Commons CSV
Apache Commons CSV on suosittu kirjasto CSV-tiedostojen käsittelyyn Javassa. Se yksinkertaistaa merkittävästi CSV-tiedostojen lukemista ja kirjoittamista.

Lisää riippuvuus projektiisi:

Mavenille:

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-csv</artifactId>
    <version>1.9.0</version> <!-- Tarkista viimeisin versio -->
</dependency>
```

#### CSV-tiedoston lukeminen:
```java
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;

import java.io.Reader;
import java.io.FileReader;
import java.io.IOException;

public class ApacheReadCSVExample {
    public static void main(String[] args) {
        String csvTiedosto = "data.csv";
        try (Reader reader = new FileReader(csvTiedosto);
             CSVParser csvParser = new CSVParser(reader, CSVFormat.DEFAULT)) {
            for (CSVRecord csvRecord : csvParser) {
                // Pääsy arvoihin sarakkeiden indeksein
                String sarakeYksi = csvRecord.get(0);
                String sarakeKaksi = csvRecord.get(1);
                System.out.println(sarakeYksi + " " + sarakeKaksi);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

#### Kirjoittaminen CSV-tiedostoon:
```java
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class ApacheWriteCSVExample {
    public static void main(String[] args) {
        String[] otsikot = {"Etunimi", "Sukunimi", "Ikä", "Kaupunki"};
        String[] data = {"John", "Doe", "30", "New York"};

        try (BufferedWriter writer = new BufferedWriter(new FileWriter("output.csv"));
             CSVPrinter csvPrinter = new CSVPrinter(writer, CSVFormat.DEFAULT.withHeader(otsikot))) {
            csvPrinter.printRecord((Object[]) data); // Tässä on tarpeen tehdä tyyppimuunnos Object[]:ksi
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Apache Commons CSV käsittelee automaattisesti monimutkaisuuksia, kuten lainausmerkit ja pilkut kentissä, tehden siitä vankan valinnan CSV:n käsittelyyn Javassa.
