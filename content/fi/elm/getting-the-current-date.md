---
title:                "Nykyisen päivämäärän hankkiminen"
html_title:           "Elm: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Mitä & Miksi? 
Päivämäärän hankkiminen tarkoittaa nykyisen päivämäärän ja ajan saamista ohjelmassa. Ohjelmoijat tekevät tämän usein tiedottaakseen käyttäjille ajankohtaisesta tiedosta tai tallentaakseen sen tietokantaan.

# Miten: 
Käytä Date.now -toimintoa hankkiaksesi nykyisen päivämäärän. Tulostaminen tapahtuu String.fromDate -funktiolla. Esimerkiksi:

```Elm
Date.now
|> String.fromDate 
```

Tulostus näyttää päivämäärän muodossa YYYY-MM-DDThh:mm:ss:ssTZ. Esimerkiksi: 2021-09-15T15:30:00.000Z

# Syväkellunta: 
Päivämäärän hankkiminen on tärkeä osa ohjelmointia, sillä se helpottaa ajankohtaisen tiedon jakamista ja tallentamista. Ennen Date.now -funktion keksimistä, ohjelmoijat joutuivat manuaalisesti hankkimaan tiedon käyttöjärjestelmältä tai muilta ulkoisilta lähteiltä. On myös olemassa muita vaihtoehtoisia ratkaisuja, kuten moment.js tai date-fns, mutta Date.now on yksinkertainen ja tehokas tapa hankkia nykyinen päivämäärä.

# Katso myös: 
- [Date.now Elm Documentation](https://package.elm-lang.org/packages/elm/time/latest/Time#now)
- [moment.js](https://momentjs.com/)
- [date-fns](https://date-fns.org/)