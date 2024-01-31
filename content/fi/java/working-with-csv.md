---
title:                "CSV-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Bash: CSV-tiedostojen käsittely"
simple_title:         "CSV-tiedostojen käsittely"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV, eli Comma-Separated Values, on tiedostomuoto, jossa data on erotettu pilkuilla. Ohjelmoijat käyttävät sitä, koska se on yksinkertainen, lukuisissa järjestelmissä suosittu ja helppo ihmisen lukea ja kirjoittaa.

## How to:
Java-koodi lukee CSV-tiedoston ja tulostaa arvot:

```java
import java.nio.file.*;
import java.io.*;
import java.util.*;

public class SimpleCSVReader {
    public static void main(String[] args) {
        Path csvFilePath = Paths.get("data.csv");

        try (BufferedReader br = Files.newBufferedReader(csvFilePath)) {
            String line;
            while ((line = br.readLine()) != null) {
                String[] values = line.split(",");
                System.out.println(Arrays.toString(values));
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Sample output:

```
["nimi", "ikä", "kaupunki"]
["Mikko", "30", "Helsinki"]
["Liisa", "25", "Espoo"]
```

## Deep Dive
CSV-tiedostot ovat olleet käytössä jo vuosikymmeniä. Monet ohjelmat, kuten taulukkolaskentaohjelmat, tukevat CSV-vientiä ja -tuontia. Vaihtoehtoina ovat esimerkiksi JSON tai XML, joissa data on rakenteisempaa. CSV:n kanssa työskennellessä huomioi merkistökoodaus (kuten UTF-8) ja kenttäerotin (pilkku ei ole aina standardi).

## See Also
- OpenCSV-kirjasto: http://opencsv.sourceforge.net/
- RFC 4180, CSV standardin dokumentaatio: https://tools.ietf.org/html/rfc4180
- Oracle Java dokumentaatio: https://docs.oracle.com/javase/8/docs/api/java/io/BufferedReader.html
