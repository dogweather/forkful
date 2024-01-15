---
title:                "Tarkistetaan löytyykö kansio"
html_title:           "C#: Tarkistetaan löytyykö kansio"
simple_title:         "Tarkistetaan löytyykö kansio"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmoinnissa tarvitsemme tietää, onko tietty kansio olemassa. Tämä voi olla tarpeellista esimerkiksi sen varmistamiseksi, että tietokoneen resursseja käytetään tehokkaasti ja turhaa aikaa ei hukata yrittäessämme päästä käsiksi olemattomaan kansioon.

## Miten tehdä se

Tarkistaaksesi, onko kansio olemassa C# -koodissa, voit käyttää "Directory.Exists()" -metodia. Tämä metodi ottaa parametriksi polun kansion sijaintiin ja palauttaa boolean-arvon osoittamaan, onko kyseinen kansio olemassa vai ei. Katso alla oleva esimerkki:

```C#
string kansio = @"C:\Tiedostot\Kansio";

if (Directory.Exists(kansio))
{
    Console.WriteLine("Kansio on olemassa!");
}
else
{
    Console.WriteLine("Kansiota ei löytynyt.");
}
```

Tässä esimerkissä luomme muuttujan kansio, joka osoittaa polkuun "C:\Tiedostot\Kansio". Sitten tarkistamme, onko kansio olemassa käyttämällä "Directory.Exists()" -metodia. Jos kansio on olemassa, tulostamme ilmoituksen "Kansio on olemassa!", muuten tulostamme "Kansiota ei löytynyt.".

## Syvällisempi sukellus

Kansioita voidaan tarkistaa myös erityisillä ehdoilla käyttäen "Directory.GetDirectories()" ja "Directory.GetDirectories()" -metodeja. Näitä metodeja voidaan käyttää etsimään tiettyä kansiorakennetta tai tietynlaista tiedostoa sisältävää kansiota. Voit tutustua näihin metodeihin tarkemmin Microsoftin dokumentaatiosta.

## Katso myös

- C# Directory-luokan dokumentaatio: https://docs.microsoft.com/en-us/dotnet/api/system.io.directory?view=net-5.0
- Directory.Exists () -metodin käyttöohjeet: https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists?view=net-5.0
- Directory.GetDirectories () -metodin käyttöohjeet: https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.getdirectories?view=net-5.0