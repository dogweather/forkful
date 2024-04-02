---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:23.304138-07:00
description: "C#:ssa t\xE4m\xE4nhetkisen p\xE4iv\xE4m\xE4\xE4r\xE4n saaminen tarkoittaa\
  \ j\xE4rjestelm\xE4st\xE4 nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n ja ajan tietojen noutamista.\
  \ Ohjelmoijat tarvitsevat usein p\xE4\xE4sy\xE4\u2026"
lastmod: '2024-03-13T22:44:56.583820-06:00'
model: gpt-4-0125-preview
summary: "C#:ssa t\xE4m\xE4nhetkisen p\xE4iv\xE4m\xE4\xE4r\xE4n saaminen tarkoittaa\
  \ j\xE4rjestelm\xE4st\xE4 nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n ja ajan tietojen noutamista.\
  \ Ohjelmoijat tarvitsevat usein p\xE4\xE4sy\xE4\u2026"
title: "Nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n hankkiminen"
weight: 29
---

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
