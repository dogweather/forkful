---
title:                "CSV-tiedostojen käsittely"
html_title:           "Bash: CSV-tiedostojen käsittely"
simple_title:         "CSV-tiedostojen käsittely"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
"Mitä ja Miksi?"

CSV-formaatti tallentaa tietoa, jossa jokainen kenttä on erotettu toisistaan pilkuilla. Ohjelmoijat käyttävät CSV:tä, koska se on yksinkertainen ja laajasti yhteensopiva eri järjestelmien kanssa.

## How to:
"Kuinka tehdä:"

```C#
// CSV-tiedoston luku
var tiedostonPolku = @"C:\esimerkki.csv";
var rivit = File.ReadAllLines(tiedostonPolku);
foreach (var rivi in rivit)
{
    var kentat = rivi.Split(',');
    Console.WriteLine($"Nimi: {kentat[0]}, Ikä: {kentat[1]}");
}

// CSV-tiedoston kirjoitus
var henkilot = new List<string[]>
{
    new string[] { "Matti Meikäläinen", "30" },
    new string[] { "Liisa Virtanen", "25" }
};
var uusiTiedosto = @"C:\uusi_esimerkki.csv";
using (var sw = new StreamWriter(uusiTiedosto))
{
    foreach (var henkilo in henkilot)
    {
        var rivi = string.Join(",", henkilo);
        sw.WriteLine(rivi);
    }
}
```

Esimerkkitulostus:
```
Nimi: Matti Meikäläinen, Ikä: 30
Nimi: Liisa Virtanen, Ikä: 25
```

## Deep Dive
"Syväsukellus"

CSV (Comma-Separated Values) on 1970-luvulta peräisin. Yksinkertaisuuden vuoksi CSV on kestänyt, vaikka XML ja JSON jne. ovat tulleet. Sen käyttö riippuu tarpeesta: pieniin, yksinkertaisiin datamäärityksiin se voi olla parempi kuin monimutkaisemmat formaatit. C#:ssa CSV:n käsittelyssä voi käyttää string-metodeja tai kirjastoja kuten CsvHelper, jolloin työ helpottuu.

## See Also
"Näe Myös"

- CsvHelper-kirjaston dokumentaatio: https://joshclose.github.io/CsvHelper/
- Microsoftin ohjeet tiedoston lukuun ja kirjoittamiseen C#:ssa: https://docs.microsoft.com/en-us/dotnet/standard/io/ 
- CSV-tiedoston määrittely RFC 4180: https://tools.ietf.org/html/rfc4180
