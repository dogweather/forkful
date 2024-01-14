---
title:                "C#: Luodaan väliaikainen tiedosto"
simple_title:         "Luodaan väliaikainen tiedosto"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi luoda väliaikaistiedosto?

Luodaan väliaikaistiedostoja usein silloin, kun tarvitaan tilapäistä tallennustilaa ohjelman suorituksen aikana. Tämä voi olla esimerkiksi tilanne, jossa ohjelman täytyy käsitellä suurta määrää dataa ja tarvitsee väliaikaisen tallennustilan sen väliaikaiseen käsittelyyn.

## Kuinka luoda väliaikaistiedosto C#:ssa

Käyttämällä System.IO.Path.GetTempFileName () metodia, voimme helposti luoda väliaikaistiedoston C#:ssa. Alla näet esimerkin, miten tämä tehdään:

```C#
string tempFile = Path.GetTempFileName();

Console.WriteLine("Luotu väliaikaistiedosto: " + tempFile);
```
Tämän koodin suorittamisen jälkeen näet konsolissa tulosteen, jossa kerrotaan luodun väliaikaistiedoston nimi.

## Syvälle väliaikaistiedostojen luomiseen

Väliaikaistiedostoja luodessa on tärkeää huomioida niiden oikea käyttö ja turvallisuus. Väliaikaistiedostot eivät nimittäin ole pysyviä, vaan ne poistetaan automaattisesti, kun ohjelma suljetaan. Tämän takia niiden käyttöön liittyy riskejä, joiden välttämiseksi kannattaa käyttää esimerkiksi using-lausetta varmistamaan tiedoston turvallinen poistaminen.

See Also:

- Microsoftin dokumetointi Path.GetTempFileName() -metodista: https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename?view=net-5.0
- Blogiartikkeli väliaikaistiedostojen käytöstä: https://docs.microsoft.com/en-us/dotnet/standard/io/creating-temporary-files