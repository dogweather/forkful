---
title:                "Tekstin ensimmäisten kirjainten suuraakkostaminen"
html_title:           "C#: Tekstin ensimmäisten kirjainten suuraakkostaminen"
simple_title:         "Tekstin ensimmäisten kirjainten suuraakkostaminen"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Ilmeisesti haluat oppia, miten merkkijonoja suurennetaan C#-ohjelmoinnissa. Saatat tarvita tätä taitoa esimerkiksi käsitellessäsi käyttäjien syötteitä tai muokatessasi tietokannasta haettuja tietoja.

## Miten

C#-kielellä merkkijonon suurennus on suhteellisen yksinkertaista. Voit käyttää joko `ToUpper()` tai `ToLower()` -funktiota, riippuen siitä haluatko suurentaa koko merkkijonon vai muuttaa sen pieniksi kirjaimiksi.

```C#
// Suurentaa koko merkkijonon
string teksti = "Tämä on esimerkki";
string suuriTeksti = teksti.ToUpper();
Console.WriteLine(suuriTeksti); // TULOSTAA: TÄMÄ ON ESIMERKKI

// Muuttaa merkkijonon pieniksi kirjaimiksi
string teksti = "TOINEN ESIMERKKI";
string pieniTeksti = teksti.ToLower();
Console.WriteLine(pieniTeksti); // TULOSTAA: toinen esimerkki
```

## Syvemmälle

Merkkijonon suurentaminen perustuu siihen, että jokaisella merkillä on oma ASCII-koodi. ASCII (American Standard Code for Information Interchange) on tavumerkistö, jossa jokaisella kirjaimella, numerolla ja erikoismerkillä on oma tietty numeroarvo.

Suurentamalla merkkijonon C#-koodin avulla, jokainen merkki tarkastetaan ja muunnetaan vastaavaan ASCII-koodiin. Tämän jälkeen ASCII-koodia vastaava merkki tulostetaan suurella kirjaimella.

## Katso myös

- [C#-dokumentaatio merkkijonon suurentamisesta](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper?view=netcore-3.1)
- [ASCII-taulukko](https://www.ascii-code.com/)