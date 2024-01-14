---
title:    "Javascript: Päivämäärän hakeminen"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Tervetuloa lukemaan tämän blogikirjoituksen, jossa puhumme Javascriptin päivämäärän hankinnasta. Monet ihmiset saattavat kysyä, miksi tarvitsemme päivämäärän hankintaa koodissa. Yksinkertaisesti sanottuna, päivämäärä on tärkeä tieto monissa ohjelmoinnin sovelluksissa, kuten laskupäivän määrittämisessä tai tapahtumien järjestämisessä aikajanalla. Joten, jos haluat oppia kuinka hakea ja käyttää nykyistä päivämäärää koodissa, jatka lukemista!

## Kuinka

Päivämäärän hankinta Javascript-ohjelmointikielellä on melko helppoa. Ensinnäkin, meidän on käytettävä Date-oliota, joka sisältää päivämäärän ja ajan tiedot. Voimme luoda uuden Date-olion seuraavasti:
```Javascript
let date = new Date();
```
Tämä luo uuden Date-olion, joka sisältää nykyisen päivämäärän ja ajan. Voimme sitten käyttää erilaisia Date-olion metodeja saadaksemme tietoa siitä, kuten päivämäärän, kuukauden ja vuoden. Esimerkiksi saadaksemme nykyisen päivämäärän, voimme käyttää seuraavaa komentoa:
```Javascript
let day = date.getDate();
```
Tämä tallentaa nykyisen päivämäärän numeron muuttujaan 'day'. Voimme myös hakea kuukauden numeron käyttämällä ```getMonth()``` -metodia ja vuoden käyttämällä ```getFullYear()``` -metodia.

Voit myös muokata ja tulostaa päivämäärän haluamassasi muodossa käyttämällä erilaisia metodeja. Esimerkiksi voit näyttää päivämäärän kirjoilleen seuraavasti:
```Javascript
console.log(`${day}. ${date.getMonth() + 1}. ${date.getFullYear()}`);
```

## Syväsukellus

Date-oliossa on myös muita hyödyllisiä metodeja päivämäärän, kuten viikonpäivän ja kellonajan, hankkimiseksi. Voit myös luoda Date-olion tietystä päivämäärästä ja ajasta, mikä tekee siitä erittäin joustavan ja hyödyllisen työkalun. Muista kuitenkin, että Date-olion metodit käyttävät paikallista aikavyöhykettä. Tämä voi aiheuttaa ongelmia, jos sovelluksesi on tarkoitettu maailmanlaajuiseen käyttöön. Tähän ongelmaan voi ratkaisuja meillä on artikkeli "Syväsukellus Date-olioon" (englanniksi).

## Katso myös

- [MDN-ohjeet Date-olion käytöstä](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools-opetusohjelma Date-olion käytöstä](https://www.w3schools.com/js/js_dates.asp)
- [Syväsukellus Date-olioon (englanniksi)](https://medium.com/javascript-in-plain-english/deep-dive-into-the-javascript-date-object-f971d3a40bf5)