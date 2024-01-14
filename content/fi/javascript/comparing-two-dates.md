---
title:    "Javascript: Kahden päivämäärän vertailu"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Miksi vertailla kaksi päivämäärää?

Vertaileminen on olennainen osa ohjelmointia, ja päivämäärien vertaileminen voi olla erittäin hyödyllistä monissa tilanteissa. Esimerkiksi jos haluat tarkistaa, onko jokin artikkeli julkaistu ennen tiettyä päivämäärää tai tarkistaa, kuinka monta päivää on kulunut edellisestä tapahtumasta, päivämäärien vertaileminen on välttämätöntä.

## Kuinka vertailla kahta päivämäärää?

Vertaileminen tapahtuu useimmiten käyttämällä Date-objektia ja sen sisältämiä metodeja. Ensimmäinen askel on luoda kaksi Date-objektia halutuille päivämäärille. Tämän jälkeen voit käyttää Date-objektien vertailumetodeja, kuten "getTime()", "getDate()" ja "getMonth()", saadaksesi tarvittavat tiedot. Lopuksi voit vertailla saatuja arvoja haluamallasi tavalla, esimerkiksi käyttäen if/else-lauseita. Alla on esimerkki kahden päivämäärän vertailusta:

```Javascript
const date1 = new Date("2020-01-01");
const date2 = new Date("2020-01-10");

if (date1.getTime() > date2.getTime()) {
  console.log("date1 on myöhempi kuin date2");
} else {
  console.log("date2 on myöhempi kuin date1");
}

// Output: date2 on myöhempi kuin date1
```

## Syvemmälle päivämäärien vertailuun

Päivämäärien vertailussa tulee ottaa huomioon, että Date-objektien vertailu perustuu niiden aikaleimoihin, eli millisekunteina mitattuun ajankohtaan tietyn päivämäärän jälkeen. Tämän takia esimerkiksi kaksi päivämäärää, jotka ovat samassa kuussa ja vuodessa, mutta eri viikolla, voivat silti olla eriarvoisia.

Tämän lisäksi on hyvä muistaa myös aikavyöhykkeiden vaikutus, sillä Date-objektit käyttävät oletuksena käyttöjärjestelmän aikavyöhykettä. Tämä voi aiheuttaa ongelmia, jos haluat vertailla päivämääriä eri aikavyöhykkeiltä.

## Katso myös

- MDN Web Docs, "Date" -https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/Date
- W3Schools, JavaScript Date - https://www.w3schools.com/jsref/jsref_obj_date.asp
- Stack Overflow, "How to compare two dates in JavaScript" - https://stackoverflow.com/questions/8529873/how-to-compare-two-dates-in-javascript