---
title:                "Kahden päivämäärän vertailu"
html_title:           "C#: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

Tiedät epäilemättä tilanteen, jossa sinun täytyy vertailla kahta eri päivämäärää C#:ssa. Tämä on hyödyllinen taito, jota tarvitaan esimerkiksi laskutuksessa tai tapahtumien ajoituksessa. Tässä artikkelissa opit kuinka voit helposti vertailla kahta päivämäärää C#:ssa.

## Kuinka

Vertaillaaksesi kahta päivämäärää C#:ssa, käytä DateTime-luokan Compare-metodia. Se palauttaa positiivisen arvon, jos ensimmäinen päivämäärä on suurempi kuin toinen, negatiivisen arvon jos ensimmäinen päivämäärä on pienempi kuin toinen ja nollan, jos päivämäärät ovat yhtä suuret. Katso esimerkki alla olevasta koodilohkosta:

```C#
DateTime date1 = new DateTime(2020, 10, 1);
DateTime date2 = new DateTime(2020, 10, 5);
int result = DateTime.Compare(date1, date2);
Console.WriteLine(result); // Tulostaa -1
```

Tässä esimerkissä date1 on pienempi kuin date2, joten Compare-metodi palauttaa negatiivisen arvon. Voit myös käyttää muita DateTime-luokan tarjoamia metodeja, kuten Equals tai CompareTo, vertaillaksesi päivämääriä. Katso lisätietoja [Microsoftin dokumentaatiosta](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.compare).

## Syvemmälle

On tärkeää huomata, että DateTime-luokka käyttää vuosilukuja välillä 1-9999, joten päivämäärät, jotka ovat ennen vuotta 1 tai jälkeen vuotta 9999 eivät ole kelvollisia. Tämä on hyvä ottaa huomioon, kun käsittelet historiallisia tietoja tai tulevaisuuden tapahtumia.

Lisäksi, jos haluat vertailla päivämääriä tarkemmin, voit käyttää TimeSpan-luokkaa, joka edustaa aikajaksoja. Katso seuraava esimerkki:

```C#
DateTime date1 = new DateTime(2020, 10, 1);
DateTime date2 = new DateTime(2020, 10, 5);
TimeSpan difference = date2 - date1;
Console.WriteLine(difference.Days); // Tulostaa 4
```

Saadaksesi lisätietoja TimeSpan-luokasta, tutustu [tähän Microsoftin dokumentaatioon](https://docs.microsoft.com/en-us/dotnet/api/system.timespan).

## Katso myös

- [DateTime-luokan dokumentaatio](https://docs.microsoft.com/en-us/dotnet/api/system.datetime)
- [TimeSpan-luokan dokumentaatio](https://docs.microsoft.com/en-us/dotnet/api/system.timespan)
- [Vertaaminen C#:ssa - Msdn foorumi](https://social.msdn.microsoft.com/Forums/en-US/502679be-b311-4f3d-ad99-8d06bc8d4b1b/date-comparison-in-c?forum=csharpgeneral)