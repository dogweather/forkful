---
title:                "Työskentely csv:n kanssa"
html_title:           "C#: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

CSV (Comma-Separated Values) on tapa tallentaa taulukkodataa, jossa arvot on eroteltu pilkulla. CSV-tiedostoja käytetään usein datan siirtämiseen ja tallentamiseen, koska niitä on helppo käsitellä ja ne ovat yhteensopivia useiden ohjelmistojen kanssa.

Ohjelmoijat käyttävät CSV-tiedostoja usein esimerkiksi tietokantojen tai muiden sovellusten kanssa. Ne ovat myös hyödyllisiä, kun halutaan tallentaa tai siirtää suuria määriä dataa, kuten Excel-taulukoita, yksinkertaisessa muodossa.

## Kuinka:

```C#
using System;
using System.IO; 

// Luetaan CSV-tiedosto ja tallennetaan arvot taulukkoon
string[] data = File.ReadAllLines("tiedostonimi.csv");

// Tulostetaan taulukon sisältö
foreach (string rivi in data) 
{ 
    Console.Write(rivi + "\n"); 
} 
```

Esimerkkilähtö CSV-tiedostolle, jossa on kaksi saraketta ja kolme riviä:

```
id,nimi
1,Anna
2,Pekka
3,Matti
```

Tulostus näyttäisi tältä:

```
id,nimi
1,Anna
2,Pekka
3,Matti
```

## Syvenny:

CSV on ollut käytössä jo vuosikymmeniä ja se on edelleen yksi yleisimmistä tavoista tallentaa ja siirtää dataa. Aiemmin se oli vielä suositumpi, koska monien ohjelmistojen välillä ei ollut yhteensopivuutta, mutta nykyään on olemassa myös muita vaihtoehtoja, kuten XML ja JSON.

CSV-tiedostoja voi luoda ja käsitellä myös muilla ohjelmointikielillä, kuten Pythonilla ja Javalla. C#-kielen StreamReader ja StreamWriter -luokkien avulla CSV-tiedostoja voi lukea ja kirjoittaa monipuolisemmin.

## Katso myös:

- [Microsoftin dokumentaatio CSV-tiedostoista] (https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/file-system/how-to-read-and-write-to-a-newly-created-data-file)
- [CSV-tiedostojen käsittely Python-kielellä] (https://docs.python.org/3/library/csv.html)