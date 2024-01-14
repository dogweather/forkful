---
title:    "C#: Päivämäärän hakeminen"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi hankkia nykyinen päivämäärä?

Tiedon hankkiminen nykyisestä päivästä on tärkeä osa C# ohjelmointia. On hyödyllistä tietää tarkka päivämäärä ja aika esimerkiksi laskelmien tai loki-tietueiden tallennusta varten.

## Kuinka hankkia nykyinen päivämäärä

Hankkia nykyisen päivämäärän C# koodissa on yksinkertaista käyttämällä `DateTime` rakennetta. Alla on esimerkki koodista, joka tulostaa nykyisen päivämäärän ja ajan konsoliin:

```C#
DateTime currentDate = DateTime.Today;
Console.WriteLine("Nykyinen päivämäärä: " + currentDate);
```

Tämä koodi tuottaa seuraavan tulosteen:

```
Nykyinen päivämäärä: 22.9.2021 00:00:00
```

Käyttämällä `DateTime` rakennetta voidaan myös hankkia tietoa vain päivämäärästä tai ajasta erikseen. Esimerkiksi:

```C#
DateTime currentDate = DateTime.Now;
Console.WriteLine("Nykyinen ajankohta: " + currentDate.TimeOfDay);
```

Tämä koodi tuottaa seuraavan tulosteen:

```
Nykyinen ajankohta: 17:09:45.1234567
```

## Syvällisempi tarkastelu

`DateTime` rakenne sisältää myös muita hyödyllisiä ominaisuuksia, kuten mahdollisuuden lisätä tai vähentää päiviä, tunteja tai minuutteja nykyisestä päivästä.

Esimerkiksi jos haluamme lisätä yhden päivän nykyiseen päivämäärään, voimme käyttää `AddDays()` metodia:

```C#
DateTime currentDate = DateTime.Today;
Console.WriteLine("Huomenna on: " + currentDate.AddDays(1));
```

Tämä koodi tulostaa seuraavan päivän päivämäärän:

```
Huomenna on: 23.9.2021 00:00:00
```

`DateTime` rakenteen avulla pystytään myös vertailemaan päivämääriä ja aikoja. Lisäksi se sisältää muun muassa metodeja aikavyöhykkeiden hallintaan ja päivämäärien muotoiluun.

## Katso myös

- [C# DateTime luokka](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [C# Time and Date -ohje](https://www.c-sharpcorner.com/blogs/time-and-date-c-sharp-programming1)