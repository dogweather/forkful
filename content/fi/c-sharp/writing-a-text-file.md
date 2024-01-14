---
title:                "C#: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittamalla tekstitiedoston pystyt tallentamaan tietoa ja käsittelemään sitä helposti koodissasi. Se on tärkeä osa monia ohjelmointitehtäviä ja voi auttaa sinua säilyttämään ja käyttämään tietoja tehokkaammin.

## Miten

Tallentaminen ja lukeminen tekstitiedostoon C#-ohjelmassa on hyvin yksinkertaista. Alla olevat koodiesimerkit näyttävät, kuinka helposti voit tehdä sen.

```C#
// Luo uusi tekstitiedosto
File.WriteAllText("tiedosto.txt", "Tämä on tekstitiedoston sisältö!");

// Lukee tekstitiedoston sisällön
string sisältö = File.ReadAllText("tiedosto.txt");
Console.WriteLine(sisältö);
```

Kun suoritat tämän koodin, näet tulosteen "Tämä on tekstitiedoston sisältö!" konsolissa. Voit myös käyttää muita tiedostoon kirjoittamisen ja lukemisen funktioita, kuten `File.AppendAllText()` ja `File.ReadAllLines()`.

## Syväsukellus

Kun kirjoitat tekstitiedostoon C#-ohjelmassa, tiedoston sijainti määritetään joko absoluuttisella tai suhteellisella tiedostopolulla. Absoluuttinen polku alkaa aina juurikansiosta, kun taas suhteellinen polku alkaa nykyisestä työhakemistosta. Voit lisätä tiedoston sijaintipolun parametrina kaikkiin tiedostoon kirjoittamisen ja lukemisen funktioihin.

Lisäksi voit asettaa erilaisia vaihtoehtoja tiedoston kirjoittamiseen, kuten `FileMode` ja `FileAccess`, jotka määrittävät, kuinka ohjelma käsittelee tiedoston avaamista ja kirjoittamista.

## Katso myös

- [C#:n FileInfo-luokka](https://docs.microsoft.com/fi-fi/dotnet/api/system.io.fileinfo?view=net-5.0)
- [C#:n kirjoitusjärjestelmäluokka](https://docs.microsoft.com/fi-fi/dotnet/api/System.IO?view=net-5.0)
- [C#-tekstitiedostot: Sovellukset ja esimerkit](https://www.c-sharpcorner.com/article/working-with-text-files-in-c-sharp/)