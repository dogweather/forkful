---
title:    "C#: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Usein ohjelmoinnissa tarvitsemme tapoja laskea päivämääriä tulevaisuudessa tai menneisyydessä. Tämä voi liittyä esimerkiksi työtehtäviin, matkustamiseen tai yksinkertaisesti vaikkapa syntymäpäiviä suunnitellessa. Hyvä uutinen on, että C#:ssa on olemassa käteviä keinoja tähän!

## Kuinka

Esimerkiksi voimme käyttää `DateTime` luokkaa apuna päivämäärien laskemisessa. Voimme luoda uuden instanssin tälle luokalle ja antaa sille arvoksi nykyisen päivämäärän käyttämällä `DateTime`-metodia `Now()`. Sitten voimme käyttää `Add()`-metodia lisätäksemme tai vähentääksemme päiviä kyseisestä päivämäärästä.

```C#
DateTime tanaan = DateTime.Now;
DateTime tulevaPaiva = tanaan.Add(TimeSpan.FromDays(10));
Console.WriteLine(tulevaPaiva);
```

Tämä koodi tulostaisi kymmenen päivän päästä olevan päivämäärän. Voimme myös käyttää `Parse()`-metodia muuttaaksemme merkkijonon päivämääräksi ja `ToString()`-metodia muuttaaksemme päivämäärän taas takaisin merkkijonoksi.

## Syvempi sukellus

C# tarjoaa myös muita tapoja laskea päivämääriä. Esimerkiksi `DayOfWeek`-enumerointia voidaan käyttää määrittämään, mikä päivä viikosta on kyseisessä päivämäärässä. Voimme myös käyttää `DateTime`-metodia `IsLeapYear()` tarkistaaksemme, onko kyseinen vuosi karkausvuosi.

## Katso myös

- [Microsoftin C# dokumentaatio](https://docs.microsoft.com/fi-fi/dotnet/csharp/language-reference/builtin-types/value-types)
- [Stack Overflow - Sulkeutuuko DateTime sulkeutuvana välillä?](https://stackoverflow.com/questions/50362454/does-datetime-include-seconds-in-between)