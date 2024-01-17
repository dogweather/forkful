---
title:                "Tiedostotiedoston kirjoittaminen"
html_title:           "C#: Tiedostotiedoston kirjoittaminen"
simple_title:         "Tiedostotiedoston kirjoittaminen"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Tekstitiedostojen kirjoittaminen on tapa tallentaa tietoa tietokoneen muistiin. Tämä on hyödyllistä ohjelmoinnissa, koska tekstitiedostoja voidaan käyttää tallentamaan esimerkiksi tekstimuotoista dataa tai muuttujien arvoja. Tällä tavalla voimme tallentaa ja lukea tietoja myös ohjelman suorituksen jälkeen.

## Miten:

```C#
// Luodaan uusi tekstitiedosto nimeltä "tiedosto.txt" ja avataan se kirjoittamista varten.
StreamWriter tiedosto = new StreamWriter("tiedosto.txt");

// Kirjoitetaan teksti tiedostoon ja suljetaan tiedosto.
tiedosto.WriteLine("Tervetuloa tekstitiedoston maailmaan!");
tiedosto.Close();

// Avataan tiedosto lukemista varten.
StreamReader lukija = new StreamReader("tiedosto.txt");

// Luetaan ja tulostetaan tiedoston sisältö konsoliin.
Console.WriteLine(lukija.ReadLine());
lukija.Close();
```

Esimerkissä luomme uuden tekstitiedoston nimeltä "tiedosto.txt" ja kirjoitamme siihen yhden rivin tekstin. Tiedoston avaaminen kirjoittamista varten tapahtuu `StreamWriter`-luokan avulla ja tiedoston lukeminen `StreamReader`-luokan avulla. Lopuksi suljemme molemmat tiedostot. Tämän jälkeen tulostamme luettavan rivin konsoliin.

## Syväsukellus:

Tekstitiedostojen kirjoittaminen on ollut käytössä jo pitkään, ja niitä käytetään edelleen monissa eri ohjelmoinnin sovellustarkoituksissa. Tiedostojen lisäksi on olemassa myös muita tapoja tallentaa ja lukea tietoa, kuten tietokantoja tai tiedostojärjestelmiä.

Tekstitiedostojen kirjoittaminen C#-ohjelmointikielellä tapahtuu `StreamWriter`-luokan avulla. Tämä luokka tarjoaa monipuoliset mahdollisuudet tiedostojen käsittelyyn, kuten tiedoston luomisen, avaamisen, kirjoittamisen ja sulkemisen. On myös tärkeää muistaa sulkea tiedosto käytön jälkeen, jotta se ei jää turhaan auki tai aiheuta virheitä muissa ohjelman osissa.

## Katso myös:

- [C# FileStream-luokka](https://docs.microsoft.com/en-us/dotnet/api/system.io.filestream?view=netcore-3.1) - toinen tapa käsitellä tiedostoja C#-ohjelmoinnissa.
- [Tiedostojen käsittely C#-ohjelmoinnissa](https://www.tut.fi/pop/fi/opetusohjeet/ohjelmointi/harjoitustyo/Tiedoston_kasittely_C-Sharp.pdf) - lisää tietoa tiedostojen käsittelystä C#-ohjelmoinnissa.