---
title:                "Törmääminen csv:n kanssa"
html_title:           "C#: Törmääminen csv:n kanssa"
simple_title:         "Törmääminen csv:n kanssa"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## Miksi

CSV-tiedostot ovat yksi yleisimmistä tiedostomuodoista, joita käytetään tietojen tallentamiseen ja jakamiseen. Tämän vuoksi on tärkeää ymmärtää, miten työskennellä CSV-tiedostojen kanssa, jotta voit tehokkaasti käsitellä tietoa ja tehdä siitä hyödyllistä.

## Kuinka

CSV-tiedostojen käsittely C#-ohjelmointikielellä on helppoa ja suoraviivaista. Käytämme tätä esimerkkina:

```
using System;
using System.IO;

class Program
{
    static void Main()
    {
        // Määritellään CSV-tiedoston polku ja nimi
        string csvFilePath = @"C:\Users\Käyttäjä\Documents\data.csv";

        // Luodaan tiedostoon yhteys
        StreamReader reader = new StreamReader(csvFilePath);

        // Luodaan muuttujat CSV-tiedoston sisällön lukemista varten
        string line;
        string[] fields;

        // Luetaan CSV-tiedostoa rivi kerrallaan
        while ((line = reader.ReadLine()) != null)
        {
            // Jaetaan rivi osiin pilkun perusteella
            fields = line.Split(',');

            // Tulostetaan ensimmäinen kenttä
            Console.WriteLine(fields[0]);
        }

        // Suljetaan yhteys tiedostoon
        reader.Close();
    }
}
```

Tämä yksinkertainen ohjelma lukee CSV-tiedoston ja tulostaa ensimmäisen kentän jokaisesta rivistä. Voit muokata koodia tarpeen mukaan lukeaksesi ja käsitelläksesi haluamiasi kenttiä.

## Syvemmälle CSV-tiedostojen käsittelyyn

CSV-tiedostojen käsittelyyn on olemassa paljon erilaisia työkaluja ja kirjastoja. Voit esimerkiksi käyttää "Microsoft.VisualBasic.FileIO" -kirjastoa, joka tarjoaa helppokäyttöisiä työkaluja CSV-tiedostojen lukemiseen ja kirjoittamiseen. Voit myös tutustua "CsvHelper" -kirjastoon, joka tarjoaa monipuolisia ominaisuuksia, kuten tiedostoon lisääminen ja poistaminen, tiedostojen yhdistäminen ja tiedoston sisällön muokkaus.

## Katso myös

- [Microsoft.VisualBasic.FileIO](https://docs.microsoft.com/en-us/dotnet/visual-basic/files/how-to-read-from-comma-delimited-text-files)
- [CsvHelper](https://joshclose.github.io/CsvHelper/)