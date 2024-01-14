---
title:                "C#: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi tehdä päivämäärästä merkkijono?

Monesti ohjelmoinnissa tulee tarvetta muuttaa päivämäärä merkkijonoksi, esimerkiksi käyttäjän nähtäväksi tai tietokantaan tallennettavaksi. Tämä tapahtuu yleensä siksi, että päivämäärän käsittelyyn merkkijono-muodossa on helppoa ja monipuolista.

## Kuinka tehdä se?

```C#
// Luodaan DateTime-olio, joka sisältää halutun päivämäärän
DateTime date = new DateTime(2020, 1, 14);

// Muunnetaan päivämäärä merkkijonoksi
string dateString = date.ToString();

// Tulos: "14.1.2020 0.00.00"
```

```C#
// Voimme myös määrittää halutun muotoilun käyttämällä ToString-metodia
string formattedDate = date.ToString("dd/MM/yyyy");

// Tulos: "14/01/2020"
```

Merkkijonoksi muutettu päivämäärä voidaan myös tallentaa muuttujaan tai tulostaa suoraan konsolille käyttäjän nähtäväksi.

```C#
// Tallennetaan merkkijono-muotoinen päivämäärä muuttujaan
string dateAsString = date.ToString("dddd, MMMM d, yyyy");

// Tulos: "tiistai, tammikuu 14, 2020"

// Tulostetaan päivämäärä suoraan konsolille
Console.WriteLine(date.ToString("yyyy-MM-dd"));

// Tulos: "2020-01-14"
```

## Syvällisempi sukellus

C#-kielellä päivämäärä-muuttujasta voidaan muodostaa merkkijono käyttämällä sen ToString-metodia. Metodi ottaa valinnaisena parametrina merkkijonon, joka määrittelee halutun muotoilun. Muotoilun avulla voidaan esimerkiksi määrittää päivämäärän näyttämisen tarkempi formaatti tai ottaa mukaan myös aika. C# tarjoaa monipuolisen valikoiman erilaisia muotoilutapoja, jotka löytyvät virallisen dokumentaation valmiista listaamista.

## Katso myös

- [DateTime-rakenne](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=netcore-3.1)
- [ToString-metodi](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring?view=netcore-3.1)
- [DateTimeFormatter-luokka](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.datetimeformatinfo?view=netcore-3.1)