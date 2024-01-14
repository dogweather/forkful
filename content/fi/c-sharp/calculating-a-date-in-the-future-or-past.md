---
title:                "C#: Tulevan tai menneen päivämäärän laskeminen"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaa laskea tulevaisuuden tai menneen päivämäärän? Se voi olla hyödyllistä esimerkiksi projektien suunnittelussa tai tulevien tapahtumien järjestämisessä. Myös tietokoneohjelmien ja -sovellusten kehittäjät saattavat tarvita tätä toiminnallisuutta.

## Miten

Laskutoimitus tulevaisuuden tai menneen päivämäärän laskemiseen on helppo toteuttaa C#-kielellä. Seuraavat koodilohkot näyttävät esimerkkejä laskutoimituksista ja niiden tulosteista.

```C#
// Laske päivämäärä +2 vuotta nykyhetkestä
DateTime tulevaPaiva = DateTime.Today.AddYears(2);
Console.WriteLine(tulevaPaiva);
// Tuloste: 23.12.2022

// Laske päivämäärä -1 kuukausi nykyhetkestä
DateTime tulevaPaiva = DateTime.Today.AddMonths(-1);
Console.WriteLine(tulevaPaiva);
// Tuloste: 23.10.2020

// Laske päivämäärä +5 päivää nykyhetkestä
DateTime tulevaPaiva = DateTime.Today.AddDays(5);
Console.WriteLine(tulevaPaiva);
//Tuloste: 28.09.2021
```

## Uppoaminen syvemmälle

C#-kielen DateTime-luokassa on monta eri metodia, jotka mahdollistavat päivämäärän laskemisen tulevaisuuteen tai menneeseen. Lisäksi luokassa on hyödyllisiä ominaisuuksia, kuten DateTime.Now, joka palauttaa nykyhetken päivämäärän ja kellonajan.

Myös erilaiset aikavyöhykkeet ja kesä- ja talviaika vaikuttavat päivämäärien laskemiseen, joten on tärkeää huolehtia siitä, että käytetään oikeaa aikavyöhykettä ja päivämäärien välisiä eroja.

## Katso myös

- [DateTime-luokka C#-dokumentaatiossa](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [Päivämäärän tarkistus C#-ohjelmoinnissa](https://www.c-sharpcorner.com/article/date-validation-in-C-Sharp/)
- [Aikavyöhykkeiden hallinta C#-ohjelmoinnissa](https://www.educative.io/edpresso/how-to-manage-time-zones-in-csharp)