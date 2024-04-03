---
date: 2024-01-20 17:31:12.316430-07:00
description: "Laskemme tulevaisuuden tai menneisyyden p\xE4iv\xE4m\xE4\xE4ri\xE4,\
  \ koska sovelluksemme tarvitsevat ajankohtaisia tietoja ajanhallintaan, muistutuksiin\
  \ tai\u2026"
lastmod: '2024-03-13T22:44:56.963276-06:00'
model: gpt-4-1106-preview
summary: "Laskemme tulevaisuuden tai menneisyyden p\xE4iv\xE4m\xE4\xE4ri\xE4, koska\
  \ sovelluksemme tarvitsevat ajankohtaisia tietoja ajanhallintaan, muistutuksiin\
  \ tai aikakatkaisuihin."
title: "Tulevan tai menneen p\xE4iv\xE4m\xE4\xE4r\xE4n laskeminen"
weight: 26
---

## Mitä & Miksi?
Laskemme tulevaisuuden tai menneisyyden päivämääriä, koska sovelluksemme tarvitsevat ajankohtaisia tietoja ajanhallintaan, muistutuksiin tai aikakatkaisuihin. Ohjelmoijat tekevät sen, jotta käyttäjät voivat suunnitella ja seurata tapahtumia suhteessa nykyhetkeen.

## Kuinka tehdään:
```Javascript
// Tänään + 7 päivää
let tanaan = new Date();
let tulevaisuudessa = new Date(tanaan);
tulevaisuudessa.setDate(tanaan.getDate() + 7);
console.log(tulevaisuudessa);  // Näyttää päivämäärän viikon päästä

// Tänään - 30 päivää
let menneisyydessa = new Date(tanaan);
menneisyydessa.setDate(tanaan.getDate() - 30);
console.log(menneisyydessa);  // Näyttää päivämäärän kuukausi sitten
```

## Syväsukellus:
Historiallisesti JavaScriptin päivämääräkäsittely on ollut hankalaa aikavyöhykkeiden ja kesäaikaan siirtymisen takia. ECMAScript 5 toi parannuksia ja ECMAScript 6 toi lisää metodeja päivämäärien käsittelyyn. Vaihtoehtoina voimme käyttää kirjastoja, kuten Moment.js tai date-fns, jotka tarjoavat rikkaammat APIt ja helpottavat monimutkaisempia päivämäärälaskelmia. Kuitenkin natiivit JavaScriptin Date-objektit ovat yleensä riittäviä peruskäyttöön ja suorituskyvyltään parempia.

## Katso myös:
- [MDN Web Docs - Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [date-fns kirjasto](https://date-fns.org/)
- [Moment.js kirjasto](https://momentjs.com/)
