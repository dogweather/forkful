---
title:                "Ajan erottaminen merkkijonosta"
html_title:           "C#: Ajan erottaminen merkkijonosta"
simple_title:         "Ajan erottaminen merkkijonosta"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Päivämäärän parsiminen merkkijonosta tarkoittaa päivämäärätiedon erottamista merkkijonosta ja muuntamista DateTime-tyyppiseksi muuttujaksi. Tätä tehdään esimerkiksi silloin, kun halutaan käsitellä käyttäjän syöttämää päivämäärätietoa tai hakea päivämäärällä tietokannasta. Tämä on tärkeää ohjelmoinnissa, jotta päivämäärätiedon käsittely olisi tarkkaa ja tehokasta.

## Miten:
Alla on esimerkki siitä, kuinka päivämäärän parsiminen voidaan tehdä C#-koodilla.
```C#
string dateString = "17.03.2021";
DateTime date = DateTime.Parse(dateString);
Console.WriteLine(date);

// Tulostaa: 17.03.2021 00:00:00
```

## Syvempää tietoa:
Päivämäärän parsimisen historia ulottuu aina 1960-luvulle, jolloin ensimmäinen yleisesti käytössä oleva päivämäärämuoto otettiin käyttöön. Nykyään on olemassa useita erilaisia tapoja parsia päivämäärää, kuten TryParse-funktio, joka palauttaa boolean-muuttujan sen mukaan, onnistuiko parsiminen vai ei. Lisäksi on olemassa myös toinen DateTime-tyypin rinnakkainen versio, DateTimeOffset, joka sisältää myös aikavyöhyketiedon.

## Katso myös:
- [Microsoftin virallinen dokumentaatio DateTime-luokasta](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [DateTime-luokan esittelyvideo](https://www.youtube.com/watch?v=hXNhdntEJKo) (englanniksi)
- [DotNetPerlsin esimerkkejä päivämäärän parsimisesta](https://www.dotnetperls.com/datetime)