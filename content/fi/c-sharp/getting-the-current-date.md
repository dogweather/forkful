---
title:    "C#: Saamalla nykyinen päivämäärä"
keywords: ["C#"]
---

{{< edit_this_page >}}

# Miksi Päivämäärä Tiedetään C#: llä

On monia käyttötarkoituksia, miksi haluamme tietää nykyisen päivämäärän ohjelmoimalla C#:lla. Se voi auttaa meitä seuraamaan aikaa, jolloin jotain tiettyä tapahtumaa tapahtui tai se voi olla osa laskentatoimintoa. Riippumatta syystä, tässä blogikirjoituksessa opimme, kuinka saada nykyinen päivämäärä käyttämällä C#:a.

## Kuinka Saada Nykyinen Päivämäärä

Voit saada nykyisen päivämäärän C#:lla käyttämällä DateTime-luokkaa. Tämä luokka tarjoaa useita eri menetelmiä ja ominaisuuksia, joilla voimme käsitellä päivämääriä ja aikoja. Tässä on esimerkki koodista, jossa laskemme nykyisen päivämäärän määrän päivinä ja tulostamme sen konsoliin:

```C#
// Luodaan uusi DateTime-objekti, joka säilyttää nykyisen päivämäärän ja ajan
DateTime nykyinenPäivämäärä = DateTime.Now;

// Lasketaan nykyisen päivämäärän määrä päivinä
int määräPäivät = nykyinenPäivämäärä.Day;

// Tulostetaan tulos konsoliin
Console.WriteLine("Nykyinen päivämäärä: " + määräPäivät + " päivää");
```

Tämän koodin tulostus näyttäisi tältä:

```
Nykyinen päivämäärä: 21 päivää
```

Tämä on vain yksi esimerkki siitä, kuinka voimme käyttää DateTime-luokkaa nykyisen päivämäärän saamiseen. Voimme myös käyttää muita luokan ominaisuuksia, kuten Month ja Year, saadaksemme lisätietoa päivämäärästä.

## Syvällisempi Sukellus

Tarjoamamme esimerkki käsittelee vain yksinkertaista tapaa saada nykyinen päivämäärä C#:lla. Voimme kuitenkin käyttää DateTime-luokkaa myös melko monimutkaisiin päivämäärämanipulaatioihin, kuten eri aikavyöhykkeiden käyttämiseen ja päivämäärien muuntamiseen eri muotoihin.

DateTime-luokassa on myös muita hyödyllisiä ominaisuuksia, kuten AddDays(), AddMonths() ja AddYears(), joilla voimme lisätä päivämääriin aikoja. Voimme myös käyttää ToString() -metodia saadaksemme päivämäärän haluamaamme muotoon.

DateTime-luokan käyttö on tärkeää monissa C#-ohjelmoinnissa ja voi säästää paljon aikaa ja vaivaa päivämäärän käsittelyssä.

## Katso Myös

- [DateTime-luokan dokumentaatio](https://docs.microsoft.com/fi-fi/dotnet/api/system.datetime?view=net-5.0)
- [Ohjelmointiopas C#:lle](https://www.tutorialspoint.com/csharp/index.htm)
- [C# DateTime-esimerkkejä](https://www.programiz.com/csharp-programming/datetime)