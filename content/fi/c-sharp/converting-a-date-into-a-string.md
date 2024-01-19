---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Go: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Päivämäärän muuttaminen merkkijonoksi on prosessi, jossa DateTime-objekti tai vastaava muunnetaan inhimillisesti luettavaksi tekstiksi. Tätä tehdään, jotta päivämäärätiedot voidaan esittää käyttäjille ymmärrettävässä muodossa tai tallentaa muotoon, joka on yhteensopiva ei-aikasarjatietojen kanssa.

## Näin se tehdään:

```C#
DateTime tänään = DateTime.Now;
string päivämääräMerkkijonona = tänään.ToString("dd.MM.yyyy");
Console.WriteLine(päivämääräMerkkijonona);
```

Tämä koodi tulostaa päivämäärän formaatissa "päivä.kuukausi.vuosi", esimerkiksi "12.04.2022".

## Syvempi sukellus:

Historiallisesti, DateTime luokka .NET Frameworkissä on tarjonnut `ToString`-menetelmän päivämäärän muuttamiseksi merkkijonoksi. Kuitenkin, eri kielialueiden välillä on eroja päivämäärämuodoissa, joten tiedon esittäminen voi vaihdella.

Vaihtoehtona, voit käyttää `DateTimeOffset`-luokkaa, joka sisältää aikavyöhyketiedon. Tämä on hyödyllistä, kun työskentelet kansainvälisten aikavyöhykkeiden kanssa.

On myös tärkeää huomata, että `ToString`-menetelmän muotoilutunnus on kirjainkoodi, joka määrittää päivämäärän esitysmuodon - kuten "dd" päiville, "MM" kuukausille ja "yyyy" vuosille.

## Katso myös:

Microsoftin virallinen ohje päivämäärän muotoilemiseen merkkijonoksi: [Formatting Dates](https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-date-and-time-format-strings)

.NET Frameworkin DateTime-luokka: [DateTime Class](https://docs.microsoft.com/en-us/dotnet/api/system.datetime)
  
Fluent DateTime in C#: [Fluent DateTime](https://github.com/FluentDateTime/FluentDateTime)