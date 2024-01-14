---
title:    "TypeScript: Kahden päivämäärän vertailu"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Miksi vertailla kahta päivämäärää?

Vertailemalla kahta päivämäärää voit helposti tarkistaa, kumpi päivä on aikaisempi tai myöhempi. Tämä voi olla erityisen hyödyllistä esimerkiksi projekteissa, joissa on tarvetta järjestellä päivämääriä tai tarkistaa, onko jokin tapahtuma jo tapahtunut.

## Miten tehdä vertailu TypeScriptillä?

Vertailu kahta päivämäärää TypeScriptillä on helppoa ja nopeaa. Seuraavassa koodiesimerkissä näytetään, miten voit verrata kahta päivämäärää ja tulostaa sen jälkeen tulos konsoliin.

```TypeScript
// Määritellään kaksi päivämäärää
const date1: Date = new Date("2021-01-01");
const date2: Date = new Date("2020-12-31");

// Verrataan päivämääriä ja tarkistetaan, kumpi on aikaisempi
if (date1 < date2) {
  console.log("Päivämäärä 1 on aikaisempi kuin päivämäärä 2");
} else if (date1 > date2) {
  console.log("Päivämäärä 1 on myöhempi kuin päivämäärä 2");
} else {
  console.log("Päivämäärät ovat samat");
}
```

Tulostus konsoliin:

```
Päivämäärä 1 on aikaisempi kuin päivämäärä 2
```

## Syvemmälle kahteen päivämäärään vertailuun

Päivämäärien vertaileminen TypeScriptillä perustuu niiden sisältämiin millisekunteihin. Tämän vuoksi vertailun tulos voi olla yllättävä, jos päivämäärät ovat saman päivän sisällä, mutta niiden millisekunnit eroavat toisistaan. Hyvä tapa välttää tämä on käyttää Date-objektin `setHours`, `setMinutes`, `setSeconds` ja `setMilliseconds` -metodeja, jotka asettavat tunnit, minuutit, sekunnit ja millisekunnit haluttuihin arvoihin.

Esimerkiksi jos haluat verrata päivämäärää 2021-01-01 kello 12:00 ja päivämäärää 2021-01-01 kello 12:30, mutta haluat huomioida vain päivämäärän ja tunnin erot, voit käyttää seuraavaa koodia:

```TypeScript
// Määritellään kaksi päivämäärää
const date1: Date = new Date("2021-01-01 12:00:00");
const date2: Date = new Date("2021-01-01 12:30:00");

// Asetetaan minuutit, sekunnit ja millisekunnit nollaan
date1.setMinutes(0);
date1.setSeconds(0);
date1.setMilliseconds(0);

date2.setMinutes(0);
date2.setSeconds(0);
date2.setMilliseconds(0);

// Verrataan päivämääriä ja tulostetaan tulos konsoliin
if (date1 < date2) {
  console.log("Päivämäärä 1 on aikaisempi kuin päivämäärä 2");
} else if (date1 > date2) {
  console.log("Päivämäärä 1 on myöhempi kuin päivämäärä 2");
} else {
  console.log("Päivämäärät ovat samat");
}
```

Tulostus konsoliin:

```
Päivämäärät ovat samat
```

## Katso myös

- [Date MDN -dokumentaatio](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Type