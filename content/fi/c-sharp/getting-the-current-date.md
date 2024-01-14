---
title:                "C#: Nykyisen päivämäärän saaminen"
simple_title:         "Nykyisen päivämäärän saaminen"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi C# -ohjelmoijat saattavat tarvita nykyisen päivämäärän tietoa ohjelmassaan. Ehkä he haluavat luoda aikaleimoja tallentamaan tietyn tapahtuman tai tiedoston luomisen päivämäärän, tai ehkä he tarvitsevat nykyisen päivämäärän vertailua varten tulevissa tapahtumissa. Riippumatta siitä, miksi tarvitset nykyisen päivämäärän, tämä blogikirjoitus auttaa sinua saamaan sen käyttäen C# -ohjelmointikieltä.

## Miten

C# -ohjelmointikielellä on useita tapoja saada nykyinen päivämäärä, ja tässä blogikirjoituksessa keskitymme kahteen yleisimpään tapaan: DateTime-objektin käyttöön ja string-muotoilun avulla.

Käyttämällä DateTime-objektia:

```C#
DateTime tänään = DateTime.Today;
Console.WriteLine(tänään);
```

Tämä koodinpätkä luo DateTime-objektin nimeltä "tänään" ja asettaa siihen tämänhetkisen päivämäärän. Sen jälkeen tulostetaan tämä päivämäärä konsoliin. Voit myös muotoilla päivämäärän haluamallasi tavalla käyttämällä "ToString" -metodia:

```C#
DateTime tänään = DateTime.Today;
string muotoiltuPäivämäärä = tänään.ToString("dd.MM.yyyy");
Console.WriteLine(muotoiltuPäivämäärä);
```

Tässä koodinpätkässä muotoillaan päivämäärä suomalaiseen tyyliin (päivä, kuukausi, vuosi) ja tulostetaan se konsoliin. Voit myös lisätä ajan sisällyttämällä "hh:mm:ss" muotoilun.

String-muotoilun avulla:

```C#
string nykyinenPäivämäärä = DateTime.Today.ToShortDateString();
Console.WriteLine(nykyinenPäivämäärä);
```

Tässä koodinpätkässä käytetään "ToShortDateString" -metodia, joka palauttaa päivämäärän lyhyessä muodossa. Voit myös käyttää muita string-muotoilun metodeja, kuten "ToLongDateString", joka palauttaa päivämäärän pitkässä muodossa.

## Syvällinen sukellus

C# -kielen DateTime-objekti on erittäin hyödyllinen väline päivämäärän ja ajan käsittelyyn. Siinä on monia hyödyllisiä ominaisuuksia, kuten "Add" ja "Subtract" -metodit, jotka mahdollistavat päivämäärän laskemisen tietyn ajanjakson verran eteen- tai taaksepäin.

Voit myös tarkastella päivämäärien vertailuja, kuten "Equals" ja "Compare" -metodeja, jotka auttavat sinua vertailemaan kahta päivämäärää.

## Katso myös

- [DateTime C# -dokumentaatio](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=netframework-4.8)
- [C# DateTime Tutorial](https://www.tutorialspoint.com/csharp/csharp_datetime.htm)
- [C# -ohjelmointikielen kotisivu](https://docs.microsoft.com/en-us/dotnet/csharp/)