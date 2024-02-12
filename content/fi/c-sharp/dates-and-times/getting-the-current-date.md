---
title:                "Nykyisen päivämäärän hankkiminen"
aliases:
- fi/c-sharp/getting-the-current-date.md
date:                  2024-02-03T19:09:23.304138-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nykyisen päivämäärän hankkiminen"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?
C#:ssa tämänhetkisen päivämäärän saaminen tarkoittaa järjestelmästä nykyisen päivämäärän ja ajan tietojen noutamista. Ohjelmoijat tarvitsevat usein pääsyä näihin tietoihin lokitukseen, aikaleimojen lisäämiseen toimintoihin tai tehtävien aikatauluttamiseen sovelluksissa, varmistaen, että toiminnot ajoitetaan tarkasti ja tiedot merkitään tarkoin aikaleimoin.

## Miten:
C# tarjoaa suoraviivaisen tavan saada nykyinen päivämäärä käyttäen `DateTime` luokkaa, joka on osa .NET Frameworkin System-nimiavaruutta. Alla oleva esimerkki osoittaa, miten saada nykyinen päivämäärä ja valinnaisesti aika.

```csharp
using System;

class Program
{
    static void Main()
    {
        // Saadaan nykyinen päivämäärä vain
        DateTime currentDate = DateTime.Today;
        Console.WriteLine(currentDate.ToString("d"));  // Tuloste: pp/kk/vvvv
        
        // Saadaan nykyinen päivämäärä ja aika
        DateTime currentDateTime = DateTime.Now;
        Console.WriteLine(currentDateTime.ToString()); // Tuloste: pp/kk/vvvv TT:mm:ss

        // Saadaan nykyinen UTC päivämäärä ja aika
        DateTime currentUtcDateTime = DateTime.UtcNow;
        Console.WriteLine(currentUtcDateTime.ToString()); // Tuloste: pp/kk/vvvv TT:mm:ss
    }
}
```

Kolmannen osapuolen kirjastojen osalta, NodaTime tarjoaa vankan vaihtoehdon päivämäärän ja ajan käsittelyyn, mukaan lukien nykyisen päivämäärän hankkiminen eri kalentereissa ja aikavyöhykkeillä.

```csharp
using NodaTime;
using System;

class Program
{
    static void Main()
    {
        // Käyttäen NodaTimea saadaksemme nykyisen päivämäärän ISO-kalenterissa
        LocalDate currentDate = SystemClock.Instance.GetCurrentInstant().InUtc().Date;
        Console.WriteLine(currentDate.ToString()); // Tuloste: vvvv-kk-pp

        // Aikavyöhykespesifisille päivämäärille
        DateTimeZone zone = DateTimeZoneProviders.Tzdb["America/New_York"];
        LocalDate currentZonedDate = SystemClock.Instance.GetCurrentInstant().InZone(zone).Date;
        Console.WriteLine(currentZonedDate.ToString()); // Tuloste: vvvv-kk-pp
    }
}
```

Tämä esittelee peruskäytön sisäänrakennetun `DateTime` luokan kanssa ja NodaTimen tarjoamat parannellut ominaisuudet, erityisesti sovelluksille, jotka edellyttävät eri aikavyöhykkeiden tai kalenterijärjestelmien käsittelyä.
