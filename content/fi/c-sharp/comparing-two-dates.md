---
title:    "C#: Vertaamalla kahta päivämäärää"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Miksi

On monia tilanteita, joissa kaksi päivämäärää täytyy vertailla toisiinsa C# -ohjelmassa. Tämä voi olla tarpeen esimerkiksi tietokannasta haettujen tietojen järjestämisessä tai käyttäjän antaman päivämäärän tarkastamisessa.

## Miten

Vertailun suorittaminen päivämäärille C# -koodissa on yksinkertaista käyttämällä "DateTime" -luokan "Compare" -metodia. Alla on esimerkki koodista ja sen tulostamasta tiedosta:

```C#
// Alustetaan kaksi päivämäärää
DateTime date1 = new DateTime(2020, 08, 31);
DateTime date2 = new DateTime(2021, 08, 31);

// Vertaillaan päivämääriä ja tallennetaan tulos muuttujaan "result"
int result = DateTime.Compare(date1, date2);

// Tulostetaan tulos konsoliin
Console.WriteLine(result); // Tulostaa -1

// "result" on negatiivinen, jos ensimmäinen päivämäärä on ennen toista, positiivinen jos toinen on ennen ensimmäistä ja 0 jos päivämäärät ovat samat
```

## Syväsukellus

Päivämäärien vertailuun liittyy muutamia tärkeitä huomioitavia seikkoja. Ensinnäkin, "Compare" -metodi ottaa huomioon päivämäärien aikavyöhykkeet, joten päivämäärät saattavat olla eri aikavyöhykkeillä, mutta silti samaan aikaan. Toiseksi, päivämäärät voivat olla null-arvoja, joten "Compare" -metodia tulee käyttää varoen ja huolellisesti.

## Katso myös

- [DateTime-luokka C#:ssa (Microsoft Docs)](https://docs.microsoft.com/fi-fi/dotnet/api/system.datetime?view=net-5.0)
- [Päivämäärämuuttujien vertailu (Stack Overflow)](https://stackoverflow.com/questions/1737519/how-to-compare-two-dates-in-c-sharp)
- [Päivämäärien vertailun hienouksia ja sudenkuoppia (Scott Allen)](https://odetocode.com/blogs/scott/archive/2004/12/22/calendar-classes-code-_2800_DayOfWeek_2D00_DayOfYear_2D00_And_2800_-2B00_Source-Code_2900_.aspx)