---
title:                "Tulevan tai menneen päivämäärän laskeminen"
html_title:           "TypeScript: Tulevan tai menneen päivämäärän laskeminen"
simple_title:         "Tulevan tai menneen päivämäärän laskeminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen tarkoittaa tietyn päivämäärän lisäämistä tai vähentämistä tietyn ajanjakson verran. Tämä voi olla hyödyllistä esimerkiksi sovelluksissa, jotka tarvitsevat määritettyjä päivämääriä toimintojensa suorittamiseen. Ohjelmoijat käyttävät päivämäärän laskemista myös ajan muuntamiseen eri aikavyöhykkeiden välillä.

## Kuinka tehdä?

TypeSciptissä päivämäärän laskeminen on helppoa Date-olion avulla. Katso esimerkit alla, kuinka lisätä tai vähentää päiviä, kuukausia tai vuosia annetusta päivämäärästä. 

```TypeScript
// Lisätään 10 päivää annettuun päivämäärään
let date = new Date(2020, 10, 15);
date.setDate(date.getDate()+10);
console.log(date); //Tulostaa "Tue Dec 25 2020"

// Vähennetään 2 kuukautta annetusta päivämäärästä
let date = new Date(2021, 2, 11);
date.setMonth(date.getMonth()-2);
console.log(date); //Tulostaa "Thu Jan 11 2021"

// Lisätään 5 vuotta annettuun päivämäärään
let date = new Date(2020, 5, 12);
date.setFullYear(date.getFullYear()+5);
console.log(date); //Tulostaa "Fri Jun 12 2025"
```

## Syvempi sukellus

Historiallisesti päivämäärän laskeminen on ollut tärkeää seuraavien päivämäärien laskemiseksi: päivämäärän lisääminen tai vähentäminen oli helpoin tapa tehdä tämä. Nykyään on olemassa muitakin vaihtoehtoja, kuten päivämäärien muuntaminen aikamerkkien avulla. Date-olion tarjoamat metodit mahdollistavat päivämäärän laskemisen tarkasti ja helposti.

## Katso myös

- [Date-olion dokumentaatio](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Muut vaihtoehdot päivämäärän laskemiseen JavaScriptissä](https://css-tricks.com/everything-you-need-to-know-about-date-in-javascript/)