---
title:    "Javascript: Päivämäärän muuttaminen merkkijonoksi"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi muuntaa päivämäärän merkkijonoksi? Tämä on yleinen tehtävä, joka syntyy kun ohjelmointiin liittyvää tietoa tallennetaan tietokantaan tai saman tiedon lukemiseen käytetään käyttöliittymässä. Päivämäärän muuttaminen merkkijonoksi mahdollistaa sen helpon säilyttämisen ja käytön.

## Miten

Muuntamalla päivämäärä merkkijonoksi Javascriptissa ei ole monimutkaista. Käytä vain Date-objektin sisäänrakennettua toString()-metodia. Katso alla olevaa koodi esimerkkiä ja sen palautusarvoja:

```Javascript
const date = new Date();
console.log(date.toString()); 

// Output: Mon Apr 27 2020 17: 08: 02 GMT + 0300(EEST)
```

Voit myös muuttaa päivämäärän haluamaasi muotoon käyttämällä toLocaleString() -metodia yhdessä parameterin kanssa, joka määrittää halutun kielen. Katso alla olevaa koodi esimerkkiä ja sen palautusarvoja:

```Javascript
const date = new Date();
const options = { year: 'numeric', month: 'long', day: 'numeric' };
console.log(date.toLocaleString('fi-FI', options)); 

// Output: 27. huhtikuuta 2020
```

Päivämäärän muuntamisella on myös mahdollista käyttää muita Date-objektin metodien tarjoamia vaihtoehtoja, kuten getFullYear(), getMonth() ja getDate(). Näiden avulla voit muokata päivämäärän merkkijonoksi juuri haluamallasi tavalla.

## Syvällisempi sukellus

Päivämäärän muuntaminen merkkijonoksi voi johtaa haasteisiin, kun tulee vastaan kansainvälisiä päivämäärän muotoja tai aikavyöhykkeitä. On tärkeää käyttää oikeita toimintoja ja säätää parametrejä oikein, jotta saadaan oikea tieto tallennettua ja näytettyä.

Date-objekti tarjoaa myös muita hyödyllisiä metodeja, kuten getTime(), joka muuntaa päivämäärän millisekunneiksi. Tämä on hyödyllistä, kun tieto tallennetaan tietokantaan tai käsitellään muilla ohjelmointikielillä.

Päivämäärän muuntaminen merkkijonoksi on yksi osa Javascript-ohjelmointia, joka vaatii tietämystä Date-objektin tarjoamista vaihtoehdoista ja sen käsittelemisestä oikealla tavalla.

## Katso myös

- [MDN Web Docs - Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools - Javascript Date](https://www.w3schools.com/js/js_date.asp)
- [GeeksforGeeks - JavaScript | Date toLocaleString() Method](https://www.geeksforgeeks.org/javascript-date-tolocalestring-method/)