---
title:                "C#: Tarkista löytyykö hakemisto"
simple_title:         "Tarkista löytyykö hakemisto"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi?

Tiedämme, että ohjelmoijana ei ole koskaan hauskaa, kun koodi ei toimi odotetusti. Yksi yleisimmistä ongelmista, joka voi johtaa tähän, on väärä kansiorakenne. Mutta älä huoli, tämän ongelman välttämiseksi voit tarkistaa, onko hakemistosi olemassa C# -koodilla.

## Miten tarkistaa, onko hakemisto olemassa

```C#
if (Directory.Exists("polku/hakemistoon/")) {
  Console.WriteLine("Hakemisto on olemassa.");
}
else {
  Console.WriteLine("Hakemistoa ei löytynyt.");
}
```

Tämä yksinkertainen koodiesimerkki tarkistaa, onko hakemisto olemassa annetulla polulla ja tulostaa sen perusteella viestin konsoliin. Käytämme Directory-luokkaa, joka löytyy System.IO -tilasta.

## Syvällisempi katsaus

Tarkemmin sanottuna tarkistus toimii tarkistamalla, onko annetulla polulla oleva hakemisto olemassa ja palauttaa sitten boolean-arvon true tai false. Jos hakemisto löytyy, niin koodissamme määritellyt komennot suoritetaan. Jos sitä ei löydy, niin voit käsitellä tilanteen eri tavoin ja esimerkiksi välttää mahdolliset virheet koodissasi.

Tärkeää on myös huomata, että tarkistus ei tarkoita vain hakemiston nimen tarkistamista, vaan myös sen, että annetussa polussa on kohde, joka on nimeltään hakemisto.

## Katso myös

- [Directory.Exists-metodi (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists)
- [System.IO -tilan dokumentaatio (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.io)

Onnea hakemistojen tarkistamiseen C#-koodilla! Toivottavasti tämä blogikirjoitus auttoi sinua paremmin ymmärtämään, miksi ja miten tarkistaa hakemistoja. Lisätietoja ja esimerkkejä löydät yllä olevista linkeistä.