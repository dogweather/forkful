---
title:                "C#: Väliaikaisen tiedoston luominen"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi luoda väliaikainen tiedosto?

Joskus ohjelmoidessa saattaa ilmetä tarve luoda väliaikaisia tiedostoja. Näitä tiedostoja käytetään yleensä tallentamaan väliaikaisia tietoja tai luomaan väliaikainen kopio olemassa olevasta tiedostosta. Ne voivat myös tarjota kätevän tavan käsitellä ja hallita tietoja eri vaiheissa ohjelman suorittamista.

## Kuinka luoda väliaikainen tiedosto C#:lla

Väliaikaisen tiedoston luominen C#:lla on helppoa ja tehokasta. Se voidaan tehdä käyttämällä .NET Frameworkin tarjoamaa `System.IO.Path` -luokkaa sekä `System.IO.File` ja `System.IO.FileStream` -luokkia. Alla on esimerkki C# -koodista, jossa luodaan väliaikainen tiedosto ja kirjoitetaan siihen tekstiä.

```C#
// Luodaan väliaikainen tiedosto
string tempFilePath = Path.GetTempFileName();

// Avataan tiedosto kirjoittamista varten
using (FileStream fs = File.Open(tempFilePath, FileMode.Open))
{
    // Luodaan StreamWriter käyttämällä FileStreamia
    using (StreamWriter writer = new StreamWriter(fs))
    {
        // Kirjoitetaan teksti tiedostoon
        writer.WriteLine("Tämä on väliaikainen tiedosto.");
    }
}

```
Tämän koodin suorittamisen jälkeen näet uuden väliaikaisen tiedoston luoto hakemistossa. Voit myös lukea tiedoston sisällön samalla tavalla, kuin lukisit tavallista tiedostoa.

## Syvällinen sukellus väliaikaisen tiedoston luomiseen

Väliaikaisen tiedoston luominen vaatii järjestelmän resursseja, joten ne olisi hyvä poistaa ohjelman suorituksen jälkeen. Tämä voidaan tehdä käyttämällä `File.Delete()` -metodia, mikä poistaa tiedoston. Jos väliaikainen tiedosto jää jotain kautta järjestelmään, sitä on mahdollista poistaa manuaalisesti myöhemmin.

Voit myös itse päättää, missä kansiossa väliaikaiset tiedostot luodaan käyttämällä `Path.GetTempPath()` -metodia. Tällä tavalla voit varmistaa, että tiedostot tallennetaan haluamaasi paikkaan.

## Katso myös

- [System.IO.Path-luokka (Microsoft)](https://docs.microsoft.com/fi-fi/dotnet/api/system.io.path)
- [System.IO.File-luokka (Microsoft)](https://docs.microsoft.com/fi-fi/dotnet/api/system.io.file)
- [System.IO.FileStream-luokka (Microsoft)](https://docs.microsoft.com/fi-fi/dotnet/api/system.io.filestream)