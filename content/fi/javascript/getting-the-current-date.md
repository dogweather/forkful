---
title:                "Javascript: Päivämäärän hankkiminen"
simple_title:         "Päivämäärän hankkiminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Javascript on yksi suosituimmista ohjelmointikielistä, ja yksi sen käyttökohteista on päivämäärän ja ajan hallinta. Saadaksemme nykyisen päivämäärän ja ajan käyttöömme, tarvitsemme Javascriptin Date-objektin avulla. Mutta miksi meidän kannattaisi edes vaivautua saamaan ne käyttöömme?

Javascriptin Date-objekti mahdollistaa monipuolisen päivämäärän ja ajan käsittelyn, kuten erilaisten muotojen muuntamisen, mutta myös tulevien päivien ja aikojen ennustamisen. Se on myös hyödyllinen esimerkiksi sovelluksissa, jotka tarvitsevat tarkkoja aikaleimoja tietyn toiminnon suorittamiseen. Tästä syystä on hyödyllistä osata käyttää Date-objektia ja hankkia nykyinen päivämäärä ja aika tarvittaessa.

## Miten

Jos olet perillä Javascriptin perusteista, tiedät varmasti, että voit luoda uuden Date-objektin käyttämällä ```new Date()``` -funktiota. Tämä luo uuden objektin, joka sisältää nykyisen päivämäärän ja ajan.

```Javascript
let nykyinenAika = new Date();
console.log(nykyinenAika);

// Tue Jan 07 2020 14:23:23 GMT+0200 (Itä-Euroopan aika)
```

Voit myös luoda Date-objektin tietyllä päivämäärällä ja ajalla antamalla parametreina vuoden, kuukauden ja päivän sekä tarvittaessa tunnin, minuutin ja sekunnin.

```Javascript
let jouluAatto = new Date(2019, 11, 24, 18, 0, 0, 0); // kuukaudet lähtevät indeksistä 0, joten joulukuu on 11
console.log(jouluAatto);

// Tue Dec 24 2019 18:00:00 GMT+0200 (Itä-Euroopan aika)
```

Voit myös käyttää Date-objektin metodeja saadaksesi haluamasi tiedot erikseen. Esimerkiksi ```getFullYear()``` palauttaa vuosiluvun ja ```getMonth()```, ```getDate()``` ja ```getDay()``` palauttavat vastaavasti kuukauden, päivän ja viikonpäivän numeromuodossa.

```Javascript
let tanaan = new Date();
let vuosi = tanaan.getFullYear();
let kuukausi = tanaan.getMonth() + 1; // lisätään 1, koska kuukaudet lähtevät indeksistä 0
let paiva = tanaan.getDate();
let paivaViikossa = tanaan.getDay(); // 0 = sunnuntai, 1 = maanantai jne.

console.log(`Tänään on ${paiva}.${kuukausi}.${vuosi} ja päivä viikossa on ${paivaViikossa}`);

// Tänään on 7.1.2020 ja päivä viikossa on 2
```

## Syvällinen sukellus

Date-objektin käyttö on kuitenkin paljon monipuolisempaa kuin pelkkä nykyisen päivämäärän ja ajan hankkiminen tai tietylle päivämäärälle ja ajalle asettaminen. Voit esimerkiksi muuttaa päivämäärän ja ajan muotoa käyttämällä erilaisia metodeita, kuten ```toDateString()```, ```toLocaleDateString()``` tai ```toString()```. Voit myös lisätä tai vähentää aikaa haluamillasi ajanjaksoilla Date-objektin metodeilla, kuten ```setDate()``` ja ```setHours()```.

Date-objektin yksi hyödyllisimmistä ominaisuuksista