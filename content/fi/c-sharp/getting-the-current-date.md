---
title:                "C#: Hankkiminen nykyinen päivämäärä"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi: Miksi hakea nykyinen päivämäärä?

Päivämäärän hakeminen on tärkeä osa monia ohjelmointiprojekteja, koska se auttaa seuraamaan aikaa ja päivämääriä. Se voi myös olla hyödyllistä tietojen tallentamisessa, järjestämisessä ja käsittelemisessä tietokannoissa. Päivämäärän haku on yksinkertainen ja tärkeä tehtävä, jota jokainen ohjelmoija tarvitsee osaamiseensa.

## Kuinka: Esimerkkejä ja tulosteita

Päivämäärän hakeminen on tehtävissä monella tavalla C# -ohjelmointikielessä. Tässä on esimerkki, joka käyttää "DateTime" -luokkaa ja sen "Today" -ominaisuutta. Tämä palauttaa nykyisen päivämäärän ja tallentaa sen muuttujaan "nykyinenPäivämäärä":

```C#
DateTime nykyinenPäivämäärä = DateTime.Today;
Console.WriteLine(nykyinenPäivämäärä);
```

Tulosteena näkyy esimerkiksi "05/03/2021 00:00:00", mikä on tämän artikkelin kirjoittamisen aikaan tämän päivän päivämäärä.

Voit myös käyttää "DateTime.Now" -ominaisuutta saadaksesi nykyisen päivämäärän ja kellonajan. Tämä palauttaa "DateTime" -objektin, jossa on tietoja tämänhetkisestä päivämäärästä ja ajasta:

```C#
DateTime nykyinenPäivämääräJaAika = DateTime.Now;
Console.WriteLine(nykyinenPäivämääräJaAika);
```

Tulosteena näkyy esimerkiksi "05/03/2021 11:12:40", mikä on tämän artikkelin kirjoittamisen aikaan tämän päivän päivämäärä ja kellonaika.

## Syväsukellus: Lisää tietoja päivämäärän hakemisesta

C# -ohjelmointikielessä on monia muita tapoja hakea nykyinen päivämäärä. Voit esimerkiksi käyttää "DateTime.UtcNow" -ominaisuutta hakiessasi päivämäärän Greenwichin aikavyöhykkeeltä tai käyttää "DateTime.Parse()" -metodia muuntaaksesi merkkijonon päivämääräksi.

Voit myös käyttää "DateTime" -luokan muita ominaisuuksia, kuten "Day", "Month" ja "Year" saadaksesi tietoa päivämäärästä. Lisäksi C# -ohjelmointikielessä on "DateTime" -luokan lisäksi muita päivämäärä- ja aikatoimintoja sisältäviä luokkia kuten "DateTimeOffset" ja "TimeSpan".

Päivämäärän hakemisen lisäksi on myös tärkeää tietää, kuinka käsitellä ja muokata päivämääriä ohjelmissa, esimerkiksi lisäämällä tai vähentämällä päiviä tai laskemalla päivien välistä eroa.

## Katso myös

- Microsoftin dokumentaatio päivämäärän hakemisesta C# -ohjelmointikielessä: https://docs.microsoft.com/fi-fi/dotnet/api/system.datetime?view=net-5.0
- Artikkeli päivämäärän muokkaamisesta C# -ohjelmointikielessä: https://www.thoughtco.com/c-sharp-datetime-manipulation-373403
- Esimerkkejä päivämäärien käsittelystä C# -ohjelmointikielessä: https://www.geeksforgeeks.org