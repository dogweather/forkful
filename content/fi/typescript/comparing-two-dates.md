---
title:    "TypeScript: Kahden päivämäärän vertaaminen"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi vertailla kahta päivämäärää?

Päivämäärien vertailu on tärkeä osa ohjelmointia, koska se mahdollistaa tietojen analysoinnin ja lajittelun perustuen ajanjaksoihin. Tämä on erityisen hyödyllistä esimerkiksi tilastodataa käsiteltäessä tai ohjelman toimintaa kontrolloitaessa ajallisesti.

## Kuinka toteuttaa päivämäärän vertailu TypeScriptillä

Vertaamalla kahta päivämäärää TypeScriptillä seuraamme yksinkertaista logiikkaa: ensin luodaan kaksi Date-objektia ja sitten vertaillaan niitä halutulla tavalla.

```TypeScript
// luodaan kaksi Date-objektia
let date1 = new Date("2021-01-01");
let date2 = new Date("2021-01-15");

// vertaillaan päivämääriä
if (date1 < date2) {
  console.log("Date1 on aiempi kuin Date2");
} else if (date2 < date1) {
  console.log("Date2 on aiempi kuin Date1");
} else {
  console.log("Päivämäärät ovat samat");
}
```

Tulostus: 
```
Date1 on aiempi kuin Date2
```

Tässä esimerkissä luomme kaksi Date-objektia käyttämällä uutta-komennon avainsanaa ja sitten vertailemme niitä if-else lauseiden avulla.

## Päivämäärän vertailun syvällisempi tarkastelu

Päivämäärien vertailussa voidaan käyttää erilaisia operaattoreita, kuten <, >, <=, ja >=, riippuen siitä, millaista vertailua halutaan tehdä. Toisinaan on myös tarpeen tarkastella päivämääriä tietyn ajanjakson sisällä, jolloin voidaan käyttää esimerkiksi Date.prototype.getTime() -metodia.

Lisäksi TypeScriptillä on käytössä neljä erilaista päivämäärän luokkaa: Date, DateTime, Time ja DateTimeOffset. Jokaisella näistä on omat ominaisuutensa ja metodinsa, jotka mahdollistavat päivämäärien vertailun halutulla tavalla.

## Katso myös

- [MDN Web Docs: Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript Docs: Date](https://www.typescriptlang.org/docs/handbook/utility-types.html#date)
- [Stack Overflow: How to compare two dates in TypeScript](https://stackoverflow.com/questions/42012021/how-to-compare-two-dates-in-typescript)