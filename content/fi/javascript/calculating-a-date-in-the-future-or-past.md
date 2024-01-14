---
title:                "Javascript: Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi joku saattaisi haluta laskea tietyn päivämäärän tulevaisuudessa tai menneisyydessä. Saattaa olla tarpeellista suunnitella tulevia tapahtumia tai muistaa menneitä tapahtumia. Matkustaessa saattaa myös olla hyödyllistä tietää, mikä päivä on tulevaisuudessa tai menneisyydessä.

## Kuinka tehdä

Matematiikkaa käyttämällä ja muutamalla yksinkertaisella Javascript-koodilla, voimme laskea halutun päivämäärän tulevaisuudessa tai menneisyydessä. Ensimmäisenä meille täytyy asettaa lähtöpäivämäärä ja sitten laskea montako päivää haluamme lisätä tai vähentää. Käytämme Date-objektia ja sen sisäänrakennettuja funktioita kuten getDate(), getMonth() ja getFullYear(), jotta voimme käsitellä päivämääriä helposti.

```Javascript 
//asetetaan lähtöpäivämäärä 
var lahtoPaiva = new Date(2021, 7, 1); 

//lasketaan montako päivää etsimämme päivämäärä on lähtöpäivämäärästä
var montakoPaivaa = 30; 

//lasketaan tulevaan päivämäärän käyttämällä getDate(), getMonth() ja getFullYear()
var tulevaPaiva = lahtoPaiva.getDate() + montakoPaivaa; 
var tulevaKuukausi = lahtoPaiva.getMonth(); 
var tulevaVuosi = lahtoPaiva.getFullYear(); 

//tulostetaan tuleva päivämäärä 
console.log("Tuleva päivämäärä: " + tulevaPaiva + "." + tulevaKuukausi+ "." + tulevaVuosi); 
```

Tämä koodi tulostaisi "Tuleva päivämäärä: 31.7.2021". Vastaavasti voimme myös laskea menneen päivämäärän vähentämällä päiviä lähtöpäivämäärästä.

## Syventävä tieto 

Javascriptin Date-objektilla on monia muita sisäänrakennettuja funktioita, joita voimme käyttää tulevien tai menneiden päivämäärien laskemisessa. Voimme myös lisätä tai vähentää muita aikayksiköitä, kuten tunteja, minuutteja tai sekunteja.

On myös hyödyllistä muistaa, että Javascript käyttää UTC (Universal Coordinated Time) aikavyöhykettä Date-objektissa. Voimme käyttää Javascriptin sisäänrakennettuja funktioita, kuten getTimezoneOffset(), muuttaaksemme UTC-aikaa paikalliseen aikaan.

## Katso myös

- [Javascript Date-objektin dokumentaatio](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools: Javascript Date-objektin esimerkkejä](https://www.w3schools.com/js/tryit.asp?filename=tryjs_date)
- [Javascriptin aikavyöhykkeiden hallinta](https://www.digitalocean.com/community/tutorials/how-to-handle-date-time-data-in-javascript)