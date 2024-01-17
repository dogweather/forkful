---
title:                "Tulevaisuuden tai menneen päivämäärän laskeminen tietokoneohjelmoinnissa"
html_title:           "Javascript: Tulevaisuuden tai menneen päivämäärän laskeminen tietokoneohjelmoinnissa"
simple_title:         "Tulevaisuuden tai menneen päivämäärän laskeminen tietokoneohjelmoinnissa"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

#Mitä & Miksi?

Laskeminen tiettyyn päivämäärään tulevaisuudessa tai menneisyydessä on yleinen ohjelmointitehtävä, joka vaatii päivämäärien käsittelyä. Tämä voi tapahtua esimerkiksi ajastimien asettamiseksi tuleville tapahtumille tai päättymispäivämäärän laskemiseksi projektin aikataulun mukaan. Ohjelmoijat joutuvat usein käyttämään päivämääriin liittyviä toimintoja tehdessään monimutkaisempia laskelmia tai aikajärjestyksessä toistuvia tehtäviä.

#Kuinka:

Jos haluat laskea päivämäärän tietyn ajanjakson päästä tai menneisyyteen, voit käyttää Date-objektin 'setDate' -metodia, joka hyväksyy parametrina päivämäärän numeron kuukaudessa ja vuoden numeron. Seuraavassa on yksinkertainen esimerkki:

```Javascript
const tulevaisuudenPaiva = new Date(); //Luodaan uusi 'Date'-objekti
tulevaisuudenPaiva.setDate(23); //Asetetaan päivämääräksi 23
```

Tämä koodi asettaa uuden päivämäärän nykyisen päiväyksen sijaan. Voit myös käyttää 'getDate' -metodia saadaksesi päivän numeron tietystä päivämäärästä:

```Javascript
const tulevaisuudenPaiva = new Date();
const paivanNumero = tulevaisuudenPaiva.getDate();
console.log(paivanNumero); //Tulostaa nykyisen päivän numeron
```

Voit myös käyttää 'getFullYear' -metodia saadaksesi vuoden numeron ja 'getMonth' -metodia saadaksesi kuukauden numeron.

#Syvemmälle:

Päivämäärien laskeminen on ollut tärkeä osa ohjelmointia jo pitkään ja se on myös yksi Javascriptin perustoiminnoista. Nykyään on olemassa myös muita vaihtoehtoja päivämäärien laskemiseen, kuten Moment.js-kirjasto. Se tarjoaa lisäominaisuuksia päivämäärien käsittelyyn ja laskemiseen.

Päivämäärien laskeminen voi olla mutkikasta, sillä eri maissa ja kulttuureissa on erilaisia tapoja esittää päivämääriä ja käyttää erilaisia kalentereita. Joten on tärkeää tarkastaa, että koodi toimii halutulla tavalla kaikissa mahdollisissa tilanteissa.

#Katso myös:

- [Date-objektin dokumentaatio MDN:ssä](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js-kirjaston kotisivut](https://momentjs.com/)