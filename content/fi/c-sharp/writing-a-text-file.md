---
title:                "Tekstitiedoston kirjoittaminen"
html_title:           "C#: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tekstitiedoston kirjoittaminen on yksi perustavanlaatuisimmista ohjelmointitaidoista, joka on tärkeä kaikille C#-ohjelmoijille. Se mahdollistaa tiedon tallentamisen ja jakamisen muille ohjelmille ja järjestelmille.

## Miten

C#-koodissa on useita eri tapoja kirjoittaa tekstitiedostoja, mutta yksinkertaisin tapa on käyttää StreamWriter-oliota. Katso alla oleva koodiesimerkki ja sen tuottama tuloste.

```C#
// Luo StreamWriter-olio ja määritä tiedoston nimi
StreamWriter writer = new StreamWriter("tekstitiedosto.txt");

// Kirjoita haluttu teksti tiedostoon
writer.WriteLine("Tämä on esimerkki tekstistä, joka tallennetaan tekstitiedostoon.");
writer.WriteLine("Voit lisätä tähän mitä vain haluat.");

// Sulje tiedosto
writer.Close();
```

Tämän koodin suorittamisen jälkeen löydät luotavan "tekstitiedosto.txt" -tiedoston samaan sijaintiin, missä koodisi on. Avaa tiedosto ja näet, että se sisältää kirjoittamasi tekstin.

## Syvemmälle

StreamWtriter-oliota käyttämällä voit myös antaa lisäparametreja, kuten tiedostomuodon ja käytetyn merkistökoodauksen. Voit myös laittaa tiedoston tiettyyn paikkaan tiedostojärjestelmässä käyttämällä koko polkua tiedoston nimeen. Katso alla oleva koodiesimerkki ja sen tuottama tuloste.

```C#
// Määritä tiedoston nimi ja tiedostonmuoto
string filePath = @"C:\Users\Käyttäjä\tiedostot\tekstitiedosto.csv";
StreamWriter writer = new StreamWriter(filePath, false, Encoding.UTF8);

// Kirjoita haluttu teksti tiedostoon
writer.WriteLine("Tämä on esimerkki tekstitiedoston kirjoittamisesta csv-muotoon.");
writer.WriteLine("Voit erottaa arvot pilkulla tai puolipisteellä.");

// Sulje tiedosto
writer.Close();
```

Tämän koodin suorittamisen jälkeen löydät luodavan "tekstitiedosto.csv" -tiedoston kansiossa, jonka olet määrittänyt. Avaa tiedosto ja näet, että se sisältää kirjoittamasi tekstin ja on muodostettu csv-tiedostoksi.

## Katso myös

Tässä muutamia resursseja, joista voit lukea lisää tekstitiedoston kirjoittamisesta C#:lla:

- [C# StreamWriter Class - Microsoft Docs](https://docs.microsoft.com/en-gb/dotnet/api/system.io.streamwriter?view=netframework-4.8)
- [How to: Write to a Text File](https://docs.microsoft.com/en-us/dotnet/standard/io/how-to-write-text-to-a-file)
- [C# Text File Examples - Dot Net Perls](https://www.dotnetperls.com/file)