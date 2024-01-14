---
title:                "TypeScript: Kahden päivämäärän vertailu"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

Vertailtujen päivämäärien vertailu voi olla todella hyödyllistä, kun haluat tarkistaa, ovatko kaksi päivämäärää samassa muodossa. Voit myös tarvita vertailla päivämääriä määrittämään, kumpi päivämäärä on aiempi tai myöhempi.

## Miten tehdä

Vertaillessa kahta päivämäärää TypeScriptissä on muutamia asioita, jotka on hyvä pitää mielessä. Helpoin tapa tehdä vertailu on muuttaa molemmat päivämäärät ensin Date-tyypiksi, jota TypeScript osaa käsitellä vertailussa.

```TypeScript
let date1: Date = new Date("June 1, 2021");
let date2: Date = new Date("July 1, 2020");

if(date1 > date2) {
  console.log("Date1 on myöhempi kuin date2.");
} else if (date1 < date2) {
  console.log("Date1 on aiempi kuin date2.");
} else {
  console.log("Date1 ja date2 ovat samoja.");
}
```

Yllä olevassa koodissa luodaan kaksi päivämäärää, jotka sitten vertaillaan if-else lauseilla. Tulostuksena saadaan tiedot siitä, kumpi on aiempi tai myöhempi, tai jos molemmat päivämäärät ovat samoja.

## Syvemmälle

Date-tyyppiä käyttämällä pystymme vertailemaan päivämääriä tarkemmin, joko vuoden, kuukauden, päivän, tunnin, minuutin tai sekunnin tarkkuudella. Esimerkiksi voit vertailla pelkästään vuosia, jolloin saat tiedon kumpi vuosi on suurempi.

```TypeScript
let date1: Date = new Date("June 1, 2021");
let date2: Date = new Date("July 1, 2020");

if(date1.getFullYear() > date2.getFullYear()) {
  console.log("Vuotta 2021 edelsi vuosi 2020.");
} else if (date1.getFullYear() < date2.getFullYear()) {
  console.log("Vuosi 2020 edelsi vuotta 2021.");
} else {
  console.log("Vuodet 2020 ja 2021 ovat samat.");
}
```

Tässä tapauksessa tulostus kertoo, että vuosi 2021 tulee ennen vuotta 2020.

## Katso myös

- [Date-tyypi TypeScriptissä](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-4.html#new-built-in-types)
- [Date-olioiden vertailu](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date#comparisons)
- [Date-vertailun mahdollisuudet](https://en.wikipedia.org/wiki/Date_comparison)