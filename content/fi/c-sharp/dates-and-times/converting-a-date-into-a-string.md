---
date: 2024-01-20 17:36:17.497638-07:00
description: "How to: (Kuinka tehd\xE4:) Alkujaan p\xE4iv\xE4m\xE4\xE4r\xE4t oli t\xE4\
  rke\xE4\xE4 esitt\xE4\xE4 kirjoitusmuodossa, koska koneet k\xE4yttiv\xE4t ep\xE4\
  ihmism\xE4isi\xE4 formaatteja. C#:ssa\u2026"
lastmod: '2024-04-05T22:51:10.740226-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4:) Alkujaan p\xE4iv\xE4m\xE4\xE4r\xE4t oli t\xE4rke\xE4\xE4\
  \ esitt\xE4\xE4 kirjoitusmuodossa, koska koneet k\xE4yttiv\xE4t ep\xE4ihmism\xE4\
  isi\xE4 formaatteja."
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n muuntaminen merkkijonoksi"
weight: 28
---

## How to: (Kuinka tehdä:)
```C#
using System;
using System.Globalization; // Tarvitaan kulttuurikohtaisiin formaatteihin

class Program
{
    static void Main()
    {
        DateTime now = DateTime.Now;

        // Oletusmuotoilu
        string defaultFormat = now.ToString();
        Console.WriteLine(defaultFormat); // "4/5/2023 11:41:00 AM"

        // Kustomoitu muotoilu
        string customFormat = now.ToString("yyyy-MM-dd");
        Console.WriteLine(customFormat); // "2023-04-05"

        // Kulttuurikohtainen muotoilu
        string finnishFormat = now.ToString("d", new CultureInfo("fi-FI"));
        Console.WriteLine(finnishFormat); // "5.4.2023"
    }
}
```

## Deep Dive (Syväsukellus):
Alkujaan päivämäärät oli tärkeää esittää kirjoitusmuodossa, koska koneet käyttivät epäihmismäisiä formaatteja. C#:ssa `DateTime`-olioita muutetaan merkkijonoiksi `.ToString()`-metodilla, jolle voi antaa parametrina formaattistringin ja/tai `CultureInfo`-olion. Formaattistring määrittelee päivämäärän esitystavan ja `CultureInfo` kulttuuririippuvaiset yksityiskohdat, kuten päivämäärän osien järjestyksen ja erotinmerkit.

Vaihtoehtoisia tapoja päivämäärän muotoilemiseen ovat esimerkiksi `String.Format` ja interpolaatio. Koodaajat voivat myös käyttää `DateTimeOffset`-olioita, jos aikavyöhykkeiden kanssa työskennellään.

Ymmärtämällä eri kulttuurien muotoilutapoja vältetään väärinymmärryksiä, sillä esimerkiksi amerikkalainen kuukausi-päivä-vuosi -formaatti voi sekoittaa suomalaiset, jotka ovat tottuneet päivä-kuukausi-vuosi -formaattiin.

## See Also (Katso Myös):
- .NET-ohjelmointi - .NET-käsikirja: [Datetime.ToString Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring)
- DateTime-muotoilun mukauttaminen: [Custom date and time format strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
- Kulttuurikohtaiset muotoilut: [CultureInfo Class](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo)
