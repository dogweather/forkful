---
title:    "TypeScript: Tulevaisuuden tai menneen päivämäärän laskeminen"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Aiheuttaessa ohjelmistossa päivämäärä ei ole aina vain tämänpäiväinen. Joskus on tarpeen laskea päivämäärä joko menneisyyteen tai tulevaisuuteen. Tähän voi olla useita syitä, esimerkiksi laskutustietojen asettaminen tietyn päivämäärän mukaan, tai tapahtumien järjestäminen tulevina päivinä. Tämä blogikirjoitus opastaa, kuinka lasket päivämäärän tulevaisuudessa tai menneisyydessä TypeScript-ohjelmoinnissa.

## Miten

Aloitetaan yksinkertaisimmasta tapauksesta: kuinka laskea päivämäärä tulevaisuudessa. Tarvitsemme käyttäjältä syötteen tulevan päivämäärän määräämiseksi ja sitä seuraavan päivän laskemiseksi:

```TypeScript
const tulevaPaiva = new Date(); // luodaan uusi päivä-olio
tulevaPaiva.setDate(tulevaPaiva.getDate() + 1); // lisätään yksi päivä
console.log(tulevaPaiva.toLocaleDateString()); // tulostetaan päivämäärä muodossa DD.MM.YYYY
```

Tämä koodilohko luo uuden päivämäärä-olion ja lisää sille yhden päivän. Lopulta tulostetaan päivämäärä halutussa muodossa. Samaa logiikkaa voi soveltaa myös menneisyydessä laskemiseen, vain muuttaen `setDate()`-funktion parametria.

Entä jos haluamme laskea tietyn määrän päiviä tulevaisuuteen tai menneisyyteen? Tällöin voimme käyttää `setDate()`-funktion sijaan `setTime()`-funktiota, jolla voimme asettaa päivämäärän millisekuntien mukaan:

```TypeScript
const tulevaPaiva = new Date();
const paivat = 5; // haluttu päivien määrä
const lisaPaivat = paivat * 24 * 60 * 60 * 1000; // muutetaan päivät millisekunneiksi
tulevaPaiva.setTime(tulevaPaiva.getTime() + lisaPaivat); // lisätään aikaa päivämäärään
console.log(tulevaPaiva.toLocaleDateString()); // tulostetaan päivämäärä muodossa DD.MM.YYYY
```

## Syvemmälle

Päivämäärän laskeminen TypeScriptissä ei kuitenkaan ole aina niin yksinkertaista. Esimerkiksi eri aikavyöhykkeillä voi olla erilainen päivämäärä ja aika. Tällöin tarvitaan `getTimezoneOffset()`-funktiota, jolla voidaan selvittää aikavyöhykkeen erotus Greenwichin aikaa vastaan ja käyttää tätä tietoa laskennassa.

Lisäksi, jos halutaan tarkka päivämäärä laskettuna esimerkiksi tietyn kuukauden ja vuoden perusteella, voi käyttää `setFullYear()`- ja `setMonth()`-funktioita. Näiden avulla voidaan asettaa haluttu vuosi ja kuukausi päivämäärään ennen laskemista.

## Katso myös

- [MDN dokumentaatio päivämäärästä ja ajasta](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript virallinen dokumentaatio](https://www.typescriptlang.org/docs/)
- [Object Oriented Tutorial](https://www.tutorialspoint.com/typescript/typescript_object_oriented.htm)

Tässä blogikirjoituksessa opimme, kuinka lasketaan päivämäärä tulevaisuudessa tai menneisyy