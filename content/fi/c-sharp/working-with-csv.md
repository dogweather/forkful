---
title:                "C#: Töiden tekeminen csv:n kanssa"
simple_title:         "Töiden tekeminen csv:n kanssa"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## Miksi käyttää CSV-tiedostoja ohjelmoinnissa?

## Kuinka tehdä CSV-tiedostoja C#-ohjelmoinnissa?

```C#
// Luodaan uusi CSV-tiedosto
using System.IO;
string filePath = "tiedosto.csv";
using (var writer = new StreamWriter(filePath))
{
    // Kirjoitetaan otsikkorivi
    writer.WriteLine("Nimi, Ikä, Sähköposti");
    // Kirjoitetaan tietueet
    writer.WriteLine("Matti, 35, matti@mail.com");
    writer.WriteLine("Liisa, 28, liisa@mail.com");
}

// Luetaan ja tulostetaan CSV-tiedoston sisältö
using (var reader = new StreamReader(filePath))
{
    string line;
    while ((line = reader.ReadLine()) != null)
    {
        Console.WriteLine(line);
    }
}

// Tulos:
// Nimi, Ikä, Sähköposti
// Matti, 35, matti@mail.com
// Liisa, 28, liisa@mail.com
```

## Syvempi sukellus CSV-tiedostoihin

CSV-tiedostot ovat erittäin kätevä tapa tallentaa taulukkomuotoisia tietoja, kuten Excel-taulukoita, ja käsitellä niitä ohjelmallisesti. Niiden käyttö ei vaadi erillisiä lisenssejä tai ohjelmia, vaan ne voidaan luoda ja lukea suoraan C#-koodilla. CSV-tiedostot ovat myös erittäin kevyitä ja helppoja jakaa eri järjestelmien välillä.

## Katso myös

- [Microsoftin ohjeet CSV-tiedostojen käsittelyyn C#-ohjelmoinnissa](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/file-system/how-to-read-and-write-to-a-newly-created-data-file)
- [CSVHelper-kirjaston käyttö C#-ohjelmoinnissa](https://joshclose.github.io/CsvHelper/)