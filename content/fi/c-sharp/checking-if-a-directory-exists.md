---
title:                "Tarkistetaan löytyykö hakemistoa"
html_title:           "C#: Tarkistetaan löytyykö hakemistoa"
simple_title:         "Tarkistetaan löytyykö hakemistoa"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Kansio-olennoista puhuttaessa, on joskus tarpeen tarkistaa, onko tietty kansio olemassa vai ei. Tätä kutsutaan kansion olemassaolon tarkistamiseksi. Ohjelmoijat tekevät näin varmistaakseen, että heidän ohjelmansa toimii oikein ja käsittelee olemassaolemattomia kansioita asianmukaisesti.

## Kuinka tehdään:
Tässä on esimerkki siitä, kuinka voit tarkistaa kansion olemassaolon C# -koodilla:

```C#
if (Directory.Exists("polku/kansio")) {
  Console.WriteLine("Kansio on olemassa.");
} else {
  Console.WriteLine("Kansiota ei löydy.");
}
```

Tuloste riippuu siitä, löytyykö annetusta polusta todellinen kansio vai ei.

## Syväsukellus:
Historiallinen konteksti: Kansion olemassaolon tarkistaminen oli tärkeämpi aiemmin, kun ohjelmat toimivat usein käyttäjän omalla koneella. Nykyään useimmat ohjelmat toimivat pilvipalvelimilla, joten kansiorakenteen tarkistaminen ei ole enää yhtä kriittistä.

Vaihtoehtoja: Voit myös käyttää System.IO.DirectoryInfo-luokkaa tarkistaaksesi olemassa olevan kansion. Tämä luokka tarjoaa lisää toimintoja, kuten kansion tiedostojen tai alikansioiden laskemista.

Tarkemmat tiedot: C# -ohjelmointikielessä on useita eri tapoja tarkistaa kansion olemassaolo, ja on tärkeää valita oikea menetelmä tarpeidesi mukaan. Kannattaa tutustua tarkempiin tietoihin ja esimerkkeihin virallisesta dokumentaatiosta.

## Katso myös:
- [System.IO.Directory-luokka](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory?view=net-5.0)
- [System.IO.DirectoryInfo-luokka](https://docs.microsoft.com/en-us/dotnet/api/system.io.directoryinfo?view=net-5.0)