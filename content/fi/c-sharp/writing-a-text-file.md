---
title:                "C#: Tekstitiedoston kirjoittaminen."
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tekstitiedoston kirjoittaminen on tärkeä osa ohjelmointia, sillä se mahdollistaa tietojen tallentamisen ja käsittelyn ohjelman suorituksen jälkeen. Tekstitiedostot ovat myös hyvä tapa kommunikoida ohjelman kanssa ja tallentaa tietoja pidemmäksi ajaksi käyttöä varten.

## Miten

Kirjoittaaksesi tekstitiedoston C#-kielellä, tarvitset System.IO-nimisen luokan, joka tarjoaa työkalut tiedostojen luomiseen, avaamiseen ja kirjoittamiseen. Alla on esimerkki koodista, joka luo ja kirjoittaa tekstitiedoston nimeltä "testi.txt":

```C#
using System.IO;

// Luo uusi tekstitiedosto
FileStream fs = File.Create("testi.txt");

// Muunnetaan string-tyyppinen tieto tavumuotoon
string teksti = "Tämä on testitiedoston teksti";
byte[] tavut = Encoding.Default.GetBytes(teksti);

// Kirjoitetaan tavut tiedostoon
fs.Write(tavut, 0, tavut.Length);
fs.Close();
```
Tämän koodin suoritus tuottaa "testi.txt"-nimisen tiedoston, jossa on teksti "Tämä on testitiedoston teksti". Voit myös käyttää StreamWriter-luokkaa tekstitiedoston kirjoittamiseen, mikä tekee prosessista hieman helpomman.

## Syvällinen sukellus

Kirjoittaessa tekstitiedostoa on tärkeää ottaa huomioon tiedoston olemassaolo ja tiedostopolku. Jos yrität kirjoittaa tiedostoa, joka jo on olemassa, se ylikirjoitetaan automaattisesti. Voit myös antaa polun, johon tiedosto sijoitetaan, kuten "C:\\Users\\Käyttäjä\\Documents\\testi.txt". Lisäksi voit käyttää StreamWriter-luokan muita metodeja, kuten WriteLine, Write ja WriteLineAsync, jotka tarjoavat enemmän joustavuutta tekstitiedoston kirjoittamiseen.

## Katso myös

Tässä artikkelissa esiteltyjen perusasioiden lisäksi tekstitiedostojen hallinnassa on paljon muita hyödyllisiä toimintoja. Voit tutustua esimerkiksi C#-ohjelmoinnin oppaisiin ja ohjelmointiympäristöihin saadaksesi lisätietoa tekstitiedostojen kirjoittamisesta ja muista hyödyllisistä tekniikoista.

[Lue lisää C# ohjelmoinnista (englanniksi)](https://docs.microsoft.com/en-us/dotnet/csharp/)

[Visual Studio -ohjelmointiympäristö (englanniksi)](https://visualstudio.microsoft.com/)