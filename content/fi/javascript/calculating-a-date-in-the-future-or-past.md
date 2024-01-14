---
title:    "Javascript: Ajan laskeminen tulevaisuudessa tai menneisyydessä"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmoinnin yhteydessä tarvitsemme tietoa tulevista tai menneistä päivistä. Tämä voi liittyä vaikkapa laskutukseen, päivämäärään perustuvaan logiikkaan tai vaikka kalenteritoimintoon. JavaScriptillä on tehokkaita työkaluja, jotka auttavat laskemaan tietyn päivämäärän tulevaisuuteen tai menneisyyteen.

## Miten tehdä

Käytä Date-objektin sisäänrakennettuja metodeja, kuten .getDate(), .getMonth() ja .getFullYear() sekä esimerkiksi matemaattisten operaatioiden, kuten yhteen- ja vähennyslaskun, avulla laskemaan tietty päivämäärä tulevaisuuteen tai menneisyyteen. Katso esimerkki koodista alla:

```Javascript
// Luodaan uusi Date-objekti, joka sisältää nykyisen päivämäärän
let currentDate = new Date();

// Lasketaan päivämäärä 30 päivää eteenpäin
let futureDate = new Date(currentDate.setDate(currentDate.getDate() + 30));

// Tulostetaan tuleva päivämäärä muodossa 'päivä.kuukausi.vuosi'
console.log(futureDate.getDate() + "." + (futureDate.getMonth() + 1) + "." + futureDate.getFullYear());

// Lasketaan päivämäärä 10 vuotta taaksepäin
let pastDate = new Date(currentDate.setDate(currentDate.getDate() - 3650));

// Tulostetaan menneisyyden päivämäärä muodossa 'vuosi, kuukausi, päivä'
console.log(pastDate.getFullYear() + ", " + (pastDate.getMonth() + 1) + ", " + pastDate.getDate());
```

Esimerkkitulostukset:

```
31.8.2021
20, 8, 2011
```

## Syvempi sukellus

Date-objekti tarjoaa myös muita hyödyllisiä metodeja, kuten .getTime(), joka palauttaa päivämäärän millisekunteina. Tämä arvo voi olla hyödyllinen, mikäli haluat vertailla päivämääriä tai laskea tarkkoja aikavälejä.

On myös huomioitava, että Date-objektin kuukaudet indeksöidään nollasta, eli tammikuu on 0 ja joulukuu on 11. Tämä tulee ottaa huomioon esimerkiksi .getMonth() -metodia käytettäessä.

## Katso myös

- [MDN web docs: Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Calculating Dates in JavaScript with Moment.js](https://www.sitepoint.com/calculating-dates-in-javascript-with-moment-js/)
- [Date arithmetic in JavaScript](https://flaviocopes.com/javascript-date-math/)