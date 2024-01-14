---
title:                "C#: Tarkistetaan, onko kansio olemassa"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi tarkistaa, onko hakemisto olemassa

On olemassa useita syitä, miksi haluat tarkistaa, onko tietty hakemisto olemassa. Tämä voi olla hyödyllistä esimerkiksi varmistaessasi, että tietty hakemisto on olemassa ennen tiedostojen tallentamista siihen, tai jos haluat tarkistaa, onko ohjelmalle tarvittavat tiedostot saatavilla ennen sen suorittamista.

## Miten tarkistaa, onko hakemisto olemassa

```C#
if (Directory.Exists("hakemisto")) // Tarkistaa, onko "hakemisto" nimistä hakemistoa olemassa
{
    Console.WriteLine("Hakemisto on olemassa.");
}
else
{
    Console.WriteLine("Hakemistoa ei löydy.");
}

// Esimerkkilähtö:
// Hakemisto on olemassa.
```

## Syvällisempi tarkastelu hakemistojen tarkistamisesta

Kun haluat tarkistaa, onko hakemisto olemassa, tärkeintä on tietää hakemistopolku ja käyttää sitä parametrina `Directory.Exists()` -metodissa. On myös tärkeää muistaa, että ohjelmalla on oltava tarvittavat oikeudet päästä hakemistoon, jotta tarkistus voi onnistua.

Voit myös käyttää `DirectoryInfo` -luokkaa hakemiston tarkistamiseen. Tämä luokka tarjoaa monia hyödyllisiä ominaisuuksia hakemistojen hallintaan, kuten tietoa koon ja luontiajan lisäksi myös `Exists` -ominaisuuden, jolla voit tarkistaa, onko hakemisto olemassa.

## Katso myös

- [Microsoftin dokumentaatio - Directory.Exists -metodi](https://docs.microsoft.com/fi-fi/dotnet/api/system.io.directory.exists?view=net-5.0)
- [Microsoftin dokumentaatio - DirectoryInfo -luokka](https://docs.microsoft.com/en-us/dotnet/api/system.io.directoryinfo?view=net-5.0)
- [C# Directory.Exists -metodi: tarkista, onko tiedosto vai hakemisto](https://www.youtube.com/watch?v=KC8gy-nrOww) (YouTube-video)