---
title:                "Väliaikaisen tiedoston luominen"
html_title:           "C#: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

Mitä ja miksi?

Väliaikaisen tiedoston luominen on yleinen käytäntö ohjelmoinnissa. Se tarkoittaa tilapäisen tiedoston luomista, joka poistetaan käytöstä ohjelman suorituksen jälkeen. Tätä tehdään usein, kun ohjelma tarvitsee tallentaa tai käsitellä tietoa, jota ei enää tarvita sen jälkeen.

Kuinka tehdä se?

C# tarjoaa useita tapoja luoda väliaikaisia tiedostoja. Yksi tapa on käyttää .NET Framework -luokkaa "Path.GetTempFileName()", joka luo ainutlaatuisen tiedostonimen ja palauttaa polun tiedostoon. Tässä on esimerkki:

```C#
string tempFile = Path.GetTempFileName();
```

Tämä koodi luo väliaikaisen tiedoston, jonka nimi on uniikki ja sijaitsee käyttöjärjestelmän osoittamassa väliaikaisessa tiedostokansiossa. Voit myös käyttää "File.Create()" -metodia luomaan ja avaamaan tiedoston kerralla:

```C#
using System.IO;

FileStream fs = File.Create(Path.GetTempFileName());
```

Voit myös itse valita väliaikaisen tiedoston sijainnin ja nimen käyttämällä "Path.Combine()" -metodia ja luomalla uuden tiedoston FileStream-luokalla:

```C#
string tempDir = Path.Combine(Path.GetTempPath(), "MyTempFiles");
string tempFile = Path.Combine(tempDir, "tempFile.txt");

using (FileStream fs = new FileStream(tempFile, FileMode.Create, FileAccess.Write))
{
    // kirjoita tai lue tiedostoon tarvittava tieto
}
```

Syöte- ja tuloste esimerkit:

> tempFile: C:\Users\käyttäjä\AppData\Local\Temp\tempFile.txt
> fs: FileStream

Syvempää tietoa

Väliaikaisten tiedostojen luomista käytetään usein sovellusten tekemiseen, jotka vaativat väliaikaista tallennustilaa tai tarvitsevat käyttää tiedostoa vain tilapäisesti. Se voi myös auttaa estämään tietojen menetyksen, kun sovellus kaatuu tai lopettaa yhteyden tietokantaan.

Vaihtoehto väliaikaisten tiedostojen käytölle on käyttää pääväylää, kuten "Environment.SpecialFolder.ApplicationData" tai "Environment.SpecialFolder.LocalApplicationData", tallentaaksesi ja käyttääksesi tiedostoja. Näitä reittejä käytetään usein sovellusten asetusten ja käyttäjän tietojen tallentamiseen.

Lisäksi on tärkeää muistaa poistaa väliaikainen tiedosto, kun sitä ei enää tarvita. Voit tehdä tämän käyttämällä "File.Delete()" -metodia:

```C#
string tempFile = Path.GetTempFileName();
// tee jotain väliaikaisella tiedostolla
File.Delete(tempFile); // poistaa tiedoston järjestelmästä
```

Katso myös

Voit lukea lisää väliaikaisten tiedostojen luomisesta ja käytöstä C#:ssa Microsoftin dokumentaatiosta: https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename?view=netframework-4.8. Voit myös tutustua muihin tiedostojen käsittelyyn liittyviin luokkiin, kuten "File", "Directory", ja "Path".