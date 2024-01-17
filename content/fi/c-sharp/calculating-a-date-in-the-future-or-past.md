---
title:                "Ajan laskeminen tulevaisuudessa tai menneisyydessä"
html_title:           "C#: Ajan laskeminen tulevaisuudessa tai menneisyydessä"
simple_title:         "Ajan laskeminen tulevaisuudessa tai menneisyydessä"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen on prosessi, jossa ohjelmoija määrittää tietyn ajanjakson kuluneen tai tulevan päivän perusteella. Tämä on hyödyllistä monissa sovelluksissa, kuten aikatauluissa, tilastojen laskennassa ja muiden tapahtumien suunnittelussa.

## Näin teet sen:
```C#
DateTime tulevaisuus = DateTime.Now.AddDays(7);
DateTime menneisyys = DateTime.Now.AddYears(-5);

Console.WriteLine(tulevaisuus);
Console.WriteLine(menneisyys);

// Output:
// 27.10.2020 10:14:18
// 28.10.2015 10:14:18
```

## Syvemmälle:
Historiallisessa kontekstissa päivämäärän laskeminen aiemmin vaati monimutkaista matematiikkaa, mutta nykyään C# tarjoaa kätevät valmiit funktiot tähän tarkoitukseen. Toisinaan myös päivämäärien vertaileminen ja tarkastelu voi olla hyödyllistä, jotta voidaan varmistua tietyn päivämäärän sijainnista tiettyjen aikarajojen sisällä.

## Katso myös:
Microsoftin dokumentaatio C# Date and Time -toiminnoista: https://docs.microsoft.com/en-us/dotnet/standard/datetime/