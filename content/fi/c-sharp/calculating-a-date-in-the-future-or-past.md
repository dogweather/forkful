---
title:                "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
html_title:           "C#: Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
simple_title:         "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Lasketaan tulevaisuuden tai menneisyyden päivämäärä tarkoittaa ajankohdan määrittämistä lisäämällä tai vähentämällä päiviä, kuukausia tai vuosia nykyisestä päivämäärästä. Ohjelmoijat tekevät tämän tehtävän, koska se on hyödyllinen aikajanaan liittyvien sovellusten käsittelyssä ja seurannassa.

## Kuinka:

Käytämme C#'sta valmiita DateTime- ja TimeSpan -luokkia näiden laskelmien toteuttamiseen. Tässä on esimerkki:

```C#
DateTime nykyhetki = DateTime.Now;
DateTime tulevaisuudessa = nykyhetki.AddDays(90);
Console.WriteLine($"Tulevaisuuden päivämäärä on: {tulevaisuudessa}");
```

Tämä koodi tulostaa tulevan päivämäärän, joka on tasan 90 päivää nykyhetkestä.

## Deep Dive:

Historiallisesti päivämäärien laskeminen on ollut osa ohjelmointia alusta alkaen. C# on tehnyt tästä yksinkertaisempaa tarjoamalla valmiita luokkia, kuten DateTime ja TimeSpan.

C#'ssa on myös muita vaihtoehtoja, kuten DateTimeOffset, joka ottaa huomioon aikavyöhykkeet. 

DateTime-sekä TimeSpan-luokkien yksityiskohtainen toiminta on seuraavanlainen: DateTime palauttaa ajankohdan ja ajan, kun taas TimeSpan edustaa ajanjakson pituutta. Yhdistämällä nämä kaksi voimme laskea tulevaisuuden tai menneisyyden päivämäärän.

## Katso myös:

 - [Microsoftin DateTime-luokka](https://docs.microsoft.com/fi-fi/dotnet/api/system.datetime?view=net-6.0)
 - [Microsoftin TimeSpan-luokka](https://docs.microsoft.com/fi-fi/dotnet/api/system.timespan?view=net-6.0)