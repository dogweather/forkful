---
title:                "Nykyisen päivämäärän hankkiminen"
date:                  2024-01-20T15:13:28.429755-07:00
html_title:           "Bash: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"

category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
"Nykyisen päivämäärän haku C#:lla"
Ohjelmointimaailmassa ajantasaisen päivämäärän hankkiminen on perustoiminto, jota käytetään aikaleimojen luomiseen, päivittäisiin tehtäviin ja tapahtumien seurantaan.

## How to:
Helppo tapa hakea nykyinen päivämäärä C#-kielellä on käyttää `DateTime`-luokkaa. Tässä on pari riviä koodia ja esimerkkituloste:

```C#
// Tämänhetkisen päivämäärän ja kellonajan saaminen
DateTime now = DateTime.Now;
// Tulosta nykyinen päivämäärä ja aika
Console.WriteLine(now.ToString());

// Tulostaa vain nykyisen päivämäärän
Console.WriteLine(now.ToShortDateString());
```

Odotettavissa oleva tuloste:
```
// Esimerkki tuloksesta (riippuu järjestelmän paikallisesta ajasta)
2.4.2023 14:56:01
2.4.2023
```

## Deep Dive
C# kehittyi Microsoftin toimesta 2000-luvun alussa, ja `DateTime` on aina ollut sen vakiokirjastoissa. Mitä vaihtoehtoja sitten on? Voisit harkita `DateTimeOffset`-luokkaa, jos tarvitset aikavyöhyketietoja tai `TimeSpan`-luokkaa, jolla mitataan ajan kulu.

Implementaation kannalta `DateTime.Now` hakee laitteen paikallisen ajan, kun taas `DateTime.UtcNow` hakee koordinoidun yleisajan (UTC). Järjestelmän kello ja aikavyöhyke määrittävät `DateTime`-arvot, joten ne eivät ole aikavyöhykeitsenäisiä. Tämä tarkoittaa, että kun työskentelet kansainvälisten projektien parissa, ajanhallinnan tulisi olla UTC-muodossa.

## See Also
- Microsoftin ohjeet `DateTime`-luokasta: [docs.microsoft.com](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
- Aikavyöhykkeet ja `DateTimeOffset`: [docs.microsoft.com](https://docs.microsoft.com/en-us/dotnet/standard/datetime/choosing-between-datetime)
- Ajanhallinta .NET:ssä: [Microsoft DevBlogs](https://devblogs.microsoft.com/dotnet/date-time-and-time-zone-enhancements-in-net-6/)
