---
title:                "Päivämäärän muuntaminen merkkijonoksi"
html_title:           "C#: Päivämäärän muuntaminen merkkijonoksi"
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Päivämäärän muuntaminen merkkijonoksi tarkoittaa päivämäärän esittämistä lukijaystävällisessä muodossa, kuten "12. maaliskuuta 2021", sen sijaan että se olisi esimerkiksi numeroina. Ohjelmoijat tekevät tämän helpottaakseen päivämäärien käyttöä ja ymmärtämistä ohjelmoinnissa.

## Miten:

Koodi esimerkkiä seuraa tulostus ```C# ... ``` koodipalikoilla.

```C#
DateTime date = new DateTime(2021, 3, 12);
// Convert date into a string
string dateString = date.ToString();
Console.WriteLine(dateString);
// Output: "12.3.2021"
// Change date format
dateString = date.ToString("dd/MM/yy");
Console.WriteLine(dateString);
// Output: "12/03/21"
```

## Syväluotaus:

Päivämäärän muuntaminen merkkijonoksi on ollut tarpeen jo pitkään, sillä ihmiset ymmärtävät helpommin päivämäärät sanana kuin numeroina. On myös olemassa muita tapoja muuntaa päivämäärä merkkijonoksi, kuten DateTime.Parse -metodi, joka muuntaa merkkijonon takaisin päivämääräksi.

Ohjelmoijilla on myös mahdollisuus muokata päivämäärän muotoa, jotta se sopii paremmin ohjelman tarpeisiin. Tämä voidaan tehdä käyttämällä "dd"(päivä) ", "MM"(kuukausi) ja "yy"(vuosi) -koodeja haluttuun järjestykseen.

## Katso myös:

- [Microsoftin DateTime-luokan dokumentaatio](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [Kuinka muuntaa päivämäärä merkkijonoksi Visual Studio Code -ohjelmassa](https://code.visualstudio.com/docs/languages/csharp#_passing-strings-with-formatting-characters)