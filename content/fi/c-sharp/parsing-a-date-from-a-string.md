---
title:                "Merkkijonosta päivämäärän jäsentäminen"
date:                  2024-01-20T15:35:05.147755-07:00
html_title:           "Bash: Merkkijonosta päivämäärän jäsentäminen"
simple_title:         "Merkkijonosta päivämäärän jäsentäminen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Kun muunnat merkkijonon päivämääräksi ('parsing a date from a string'), muutat tekstissä esitettyä päivämäärätietoa päivämäärä-objektiksi. Tämä mahdollistaa päivämäärän vertailun, laskennan ja muokkauksen ohjelmassasi.

## How to: (Kuinka tehdä:)
C# tarjoaa `DateTime` luokan päivämäärän parsimiseksi. Esimerkiksi, käytä `Parse` tai `TryParse` metodia:

```C#
using System;
using System.Globalization;

class DateParsingExample
{
    static void Main()
    {
        string dateStr = "24.12.2023";
        DateTime dateTime;

        if (DateTime.TryParse(dateStr, out dateTime))
        {
            Console.WriteLine(dateTime); // Output: 24.12.2023 00:00:00
        }
        else
        {
            Console.WriteLine("Invalid date format");
        }
    }
}
```

## Deep Dive (Syväsukellus):
Päivämäärän parsiminen C#:ssa on suoraviivaista, mutta ei aina suoraviivaista. Esimerkiksi, `Parse` heittää poikkeuksen virheellisillä tiedoilla, kun taas `TryParse` palauttaa `bool`, mikä kertoo onnistuiko parsiminen.

Historiallisesti eri kulttuurit näyttävät päivämäärät eri formaateissa. C# tukee `CultureInfo`-luokkaa, joka huomioi kulttuurikohtaiset erot.

Vaihtoehtoja? Voit käyttää myös `DateTimeOffset` tai kolmannen osapuolen kirjastoja, kuten NodaTime, monimutkaisempiin aikavyöhykkeiden hallintaan.

Tärkeä yksityiskohta: virheenkäsittely ja validointi on välttämätöntä, koska käyttäjäsyötteet ovat arvaamattomia.

## See Also (Katso myös):
- Microsoftin dokumentaatio `DateTime`: [DateTime Struct (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=netcore-3.1)
- Kulttuurikohtainen päivämäärän käsittely: [CultureInfo Class (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=netcore-3.1)
- NodaTime-kirjasto ajanhallintaan: [NodaTime](https://nodatime.org/)
