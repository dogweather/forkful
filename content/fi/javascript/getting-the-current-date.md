---
title:                "Nykyisen päivämäärän saaminen"
html_title:           "Javascript: Nykyisen päivämäärän saaminen"
simple_title:         "Nykyisen päivämäärän saaminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Päivämäärän saaminen on yksinkertainen tapa tietää tämänhetkinen päivämäärä. Tämä on tärkeää monessa ohjelmointikielessä esimerkiksi aikaleimojen luomisessa tai ajan laskemisessa.

## Kuinka se tehdään?
Voit saada nykyisen päivämäärän JavaScriptillä käyttäen Date-objektia. Voit luoda uuden Date-objektin ilman parametreja jolloin se antaa nykyisen ajan ja päivämäärän, esim:
```Javascript
new Date()
```
Tämä palauttaa objektin jokaisessa käyttämässäsi muodossa, joten voit käyttää sopivaa metodista palauttaaksesi haluamasi arvon, esim:
```Javascript
new Date().getFullYear() //2021
```

## Syvällinen sukellus
Date-objekti luotiin JavaScriptin alkuperäisessä versiossa vuonna 1995. Se on ollut yksi tärkeimmistä tavoista käsitellä aikaa ja päivämäärää ohjelmoinnissa. On myös olemassa muita tapoja saada päivämäärä, kuten Moment.js-kirjaston käyttäminen tai ulkoisia API-palveluita. Date-objektin tarkka määritelmä ja sen toimintatapa voi vaihdella selaimen ja ympäristön mukaan.

## Katso myös
- [MDN Date-objekti](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)
- [WorldTime API](http://worldtimeapi.org/)