---
title:                "Päivämäärän jäsentäminen merkkijonosta"
html_title:           "Javascript: Päivämäärän jäsentäminen merkkijonosta"
simple_title:         "Päivämäärän jäsentäminen merkkijonosta"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mikä ja Miksi?

Päivämäärän jäsentäminen merkkijonosta tarkoittaa merkkijonossa olevan päivämäärän tiedon tunnistamista ja sen muuttamista koneen ymmärtämään muotoon. Koodaajat tarvitsevat tätä toiminnallisuutta, koska se on tehokas tapa lukea, esittää ja käsitellä päivämääriä eri ohjelmissa.

## Näin se tehdään:

Katsotaan, kuinka päivämäärä jäsentää merkkijonosta TypeScriptissä.

```TypeScript 
    const dateStr: string = "2022-11-21T14:12:00Z";
    let parsedDate: Date = new Date(dateStr);
    console.log(parsedDate);
```

Koodibalokki luo merkkijonon `dateStr` sisältäen päivämäärän (iso 8601 -formaatti), jäsentelee sen `Date` -objektiksi `parsedDate`, ja tulostaa sen. Tulos luetaan seuraavasti:

``` 
    2022-11-21T14:12:00.000Z
```

## Syvällisemmin

- Historiallinen asiayhteys: JavaScriptin päivämääräobjekti tarjoaa helpon työkalun päivämäärien käsittelyyn. TypeScript esiintyi myöhemmin, ja käyttää samaa lähestymistapaa, mutta tarkemmalla tyypillä.

- Vaihtoehdot: Moment.js on yksi tunnetuimmista JavaScript-päivämäärä- ja aikakirjastoista. Se sisältää tehokkaita toimintoja päivämäärien jäsentämiseen, manipulointiin ja vertailuun.

- Toteutustiedot: JavaScriptin (ja siten TypeScriptin) `Date` -konstruktori voi jäsentää ISO 8601 -muotoisia merkkijonoja oletuksena. Muun muotoiset merkkijonot vainotustuvat paikallisiksi päivämääräksi.

## Katso myös

- MDN Web Docs, [Date](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/Date) - Tietoa JavaScriptin Date-objektista.

- [Moment.js](https://momentjs.com/docs/) - Moment.js-kirjasto, joka tarjoaa kattavat työkalut päivämäärän manipulointiin.

- [ISO 8601](https://fi.wikipedia.org/wiki/ISO_8601) - Tietoa ISO8601-päivämäärästandardista.