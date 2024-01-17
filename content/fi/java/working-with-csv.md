---
title:                "Työskentely csv:n kanssa"
html_title:           "Java: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/working-with-csv.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

CSV (comma-separated values) on tapa tallentaa ja käsitellä taulukoita tekstitiedostoina. Se on suosittu formaatti, koska se on helppo luoda ja käsitellä ohjelmallisesti. Ohjelmoijat käyttävät CSV:itä muun muassa datan tallentamiseen ja vaihtoon, koska se on yksinkertainen ja luettava formaatti.

## Miten:

```Java
import java.io.*;
import java.util.*;

public class CSVReader {
  public static void main (String[] args){
    //luo uusi tiedosto-olio ja määritä polku
    File file = new File("tiedosto.csv");
    
    //luo uusi lukija-olio 
    Scanner csvReader = new Scanner(file);

    //käy läpi tiedoston rivi kerrallaan
    while (csvReader.hasNextLine()) {
        //lue rivi ja tallenna se muuttujaan
        String row = csvReader.nextLine();

        //pilko rivi osiin pilkuilla
        String[] data = row.split(",");

        //tulosta tiedot 
        for (String datum : data) {
            System.out.print(datum + " ");
        }
        System.out.println();
    }
  
    //sulje lukija 
    csvReader.close(); 
  }
}
```
Tiedoston sisältö:
```csv
Nimi,Ikä,Asuinpaikka
Matti,25,Tampere
Anna,30,Helsinki
```

Tuloste:
```
Nimi Ikä Asuinpaikka 
Matti 25 Tampere 
Anna 30 Helsinki
```

## Syvempi sukellus:

CSV-formaatti kehitettiin alun perin taloudelliseen käyttöön, mutta nykyään sitä käytetään laajasti eri ohjelmointialustoilla. Vaihtoehtoja CSV:lle ovat muun muassa JSON ja XML. CSV-tiedostoja voi avata esimerkiksi tekstieditorilla ja niitä voi luoda ja muokata myös Excelillä. 

## Katso myös:

- [CSV (Wikipedia)](https://fi.wikipedia.org/wiki/CSV)
- [Java CSV-kirjasto](https://sourceforge.net/projects/javacsv/)