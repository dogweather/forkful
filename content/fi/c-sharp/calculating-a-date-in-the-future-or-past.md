---
title:                "C#: Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen"
simple_title:         "Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmoinnissa tarvitaan laskemaan tietty päivämäärä menneisyydessä tai tulevaisuudessa. Tämä voi tapahtua esimerkiksi laskutaulukoiden luomiseksi tai päivämääräperusteisten muistutusten tekemiseksi.

## Miten

```C#
using System;

DateTime tanaan = DateTime.Today;
DateTime tulevaPaiva = tanaan.AddMonths(3); // lisätään kolme kuukautta nykyiseen päivämäärään
DateTime menneemPaiva = tanaan.AddYears(-2); // vähennetään kaksi vuotta nykyisestä päivämäärästä

Console.WriteLine("Päivämäärä kolmen kuukauden päästä: " + tulevaPaiva.ToShortDateString());
Console.WriteLine("Päivämäärä kaksi vuotta sitten: " + menneemPaiva.ToShortDateString());
```

Tulostus:

```
Päivämäärä kolmen kuukauden päästä: 01.03.2022
Päivämäärä kaksi vuotta sitten: 22.09.2019
```

Voit myös asettaa tietyn päivämäärän laskentaan käyttämällä `DateTime`-parametrejä:

```C#
DateTime syntymapaiva = new DateTime(1995, 1, 15); // päivämäärä syntymäpäivälleni
DateTime tulevaSynttari = syntymapaiva.AddYears(1); // lisätään yksi vuosi syntymäpäivään

Console.WriteLine("Seuraava syntymäpäivä: " + tulevaSynttari.ToShortDateString());
```

Tulostus:

```
Seuraava syntymäpäivä: 15.01.2022
```

## Syvällinen sukellus

Voit myös laskea päivämäärän tietynä päivänä viikossa käyttämällä `DayOfWeek`-enumerointia ja `AddDays`-metodia:

```C#
DateTime tanaan = DateTime.Today;
DateTime tulevaMaanantai = tanaan.AddDays(8 - (int)tanaan.DayOfWeek); // lisätään päiviä nykyiseen päivämäärään niin, että seuraava maanantai saadaan
DateTime tulevaPerjantai = tanaan.AddDays(12 - (int)tanaan.DayOfWeek); // lisätään päiviä niin, että seuraava perjantai saadaan

Console.WriteLine("Seuraava maanantai: " + tulevaMaanantai.ToShortDateString());
Console.WriteLine("Seuraava perjantai: " + tulevaPerjantai.ToShortDateString());
```

Tulostus:

```
Seuraava maanantai: 04.10.2021
Seuraava perjantai: 08.10.2021
```

Käytännöllisiä `DateTime`-metodeja ja enumerointeja löytyy lisää Microsoftin dokumentaatiosta.

## Katso myös

- [Microsoftin DateTime-dokumentaatio](https://docs.microsoft.com/fi-fi/dotnet/api/system.datetime?view=net-5.0)
- [Perusohjeet päivämäärien laskemiseen C#-kielellä](https://keystrokecountdown.com/blog/2019/11/05/c-sharp-date-calculations/) (englanniksi)