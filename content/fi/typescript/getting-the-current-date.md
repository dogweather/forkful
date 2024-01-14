---
title:                "TypeScript: Nykyisen päivämäärän hakeminen."
simple_title:         "Nykyisen päivämäärän hakeminen."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Aloita päiväsi TypeScriptilla! Päivämäärän hakeminen on yksi yleisimmistä ohjelmointitehtävistä ja se on erityisen hyödyllinen, kun haluat tallentaa tai näyttää tämänhetkisen päivämäärän.

## Miten

```TypeScript
// Luodaan uusi Date-olio
const tänään: Date = new Date();

// Käytetään Date-olion metodeja saadaksemme haluttu muotoiltu päivämäärä
console.log(tänään.getDate() + "." + (tänään.getMonth()+1) + "." + tänään.getFullYear());
// Tulostaa esimerkiksi 22.4.2021

// Käytetään toista metodia saadaksemme päivämäärän ja ajan
console.log(tänään.toLocaleString());
// Tulostaa esimerkiksi "22.4.2021, 11:30:12"
```

## Syvällinen tarkastelu

Päivämäärän hakeminen liittyy vahvasti Date-olion toimintaan TypeScriptissä. Date-olion avulla voit tarkastella ja muokata päivämääriä ja aikoja. Date-olion konstruktori voi ottaa parametreikseen päivämäärän, kuukauden, vuoden ja/tai ajan, mutta jos et anna sille mitään parametreja, se käyttää automaattisesti nykyhetkeä. Date-olion metodeilla voit tarkastella ja manipuloida päivämäärää ja aikaa haluamallasi tavalla. Lisätietoa Date-oliosta ja sen mahdollisuuksista voit lukea [virallisesta dokumentaatiosta.](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)

## Katso myös

- [Date-olion virallinen dokumentaatio](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScriptin perusteet](https://www.typescriptlang.org/docs/handbook/basic-types.html)
- [Vinkkejä TypeScriptin käyttöön](https://www.freecodecamp.org/news/how-to-crush-it-in-typescript-a585cbd0437a/)
- [JavaScriptin ajastinten käyttö Date-olion kanssa](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Timers)