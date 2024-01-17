---
title:                "Saat nykyisen päivämäärän"
html_title:           "C#: Saat nykyisen päivämäärän"
simple_title:         "Saat nykyisen päivämäärän"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Nykyisen päivämäärän hakeminen on yksinkertainen prosessi, jossa ohjelmoija pyytää tietokonetta näyttämään nykyisen päivämäärän ja kellonajan. Tämä on hyödyllistä, koska se auttaa ohjelmoijaa seuraamaan ajan kulumista ja tekemään päätöksiä tulevaisuudessa.

## Miten:
```C#
DateTime nykyinenPaiva = DateTime.Now;
Console.WriteLine(nykyinenPaiva.ToString());
```
**Tuloste:**
`14.9.2021 8:30:00`

## Syvä Sukellus:
Nykyisen päivämäärän hakemisen toiminto on ollut olemassa jo pitkään ohjelmoinnin alussa. Aiemmin oli tarpeen käyttää monimutkaisempia koodirivejä saadakseen päivämäärän näkymään. Nykyään C#:n `DateTime`-luokalla on sisäänrakennettu `Now`-toiminto, joka tekee tämän prosessin paljon helpommaksi.

On myös muita tapoja saada nykyinen päivämäärä. Toinen vaihtoehto on käyttää `DateTime.UtcNow`-toimintoa, joka näyttää kellonajan maailmanlaajuisesti koordinoitua aikaa (UTC). Kehittäjät voivat myös käyttää erilaisia ​​kirjastoja ja lisäosia, jotka tarjoavat muita tapoja hakea aikaa ja päivämäärää.

Nykyisen päivämäärän hakeminen on tärkeä osa ohjelmointia, koska se auttaa kehittäjiä seuraamaan aikaa ja luomaan toimintoja, jotka perustuvat päivämääriin ja aikaan. Se on myös hyödyllistä käyttäjien kannalta, kun he haluavat tietää, milloin tietty toiminto on tapahtunut.

## Katso myös:
- [DateTime.Now Property in C# (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.now?view=net-5.0)
- [DateTime.UtcNow Property in C# (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.utcnow?view=net-5.0)
- [Date and Time in C# (W3Schools)](https://www.w3schools.com/cs/cs_dates.asp)