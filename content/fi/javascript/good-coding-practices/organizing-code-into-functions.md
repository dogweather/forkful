---
date: 2024-01-26 01:10:42.365157-07:00
description: "Kuinka tehd\xE4: Perinteisesti, imperatiiviset ohjelmointikielet kuten\
  \ alkuper\xE4iset BASIC- tai Assembly-versiot eiv\xE4t tarjonneet samaa abstraktiota\
  \ kuin mit\xE4\u2026"
lastmod: '2024-04-05T21:53:58.533559-06:00'
model: gpt-4-1106-preview
summary: "Perinteisesti, imperatiiviset ohjelmointikielet kuten alkuper\xE4iset BASIC-\
  \ tai Assembly-versiot eiv\xE4t tarjonneet samaa abstraktiota kuin mit\xE4 funktiot\
  \ tarjoavat."
title: "Koodin j\xE4rjest\xE4minen funktioihin"
weight: 18
---

## Kuinka tehdä:
```javascript
// Määritellään funktio suorakulmion pinta-alan laskemiseksi
function calculateArea(width, height) {
  return width * height;
}

// Kutsutaan funktiota ja tulostetaan tulos
let area = calculateArea(5, 3);
console.log(area); // Tuloste: 15
```

```javascript
// Ryhmä suorituskykyisiin toimintoihin käyttäen funktioita
function greet(name) {
  console.log(`Hei, ${name}!`);
}

function farewell(name) {
  console.log(`Näkemiin, ${name}!`);
}

greet('Alice'); // Tuloste: Hei, Alice!
farewell('Bob'); // Tuloste: Näkemiin, Bob!
```

## Syväsukellus
Perinteisesti, imperatiiviset ohjelmointikielet kuten alkuperäiset BASIC- tai Assembly-versiot eivät tarjonneet samaa abstraktiota kuin mitä funktiot tarjoavat. Ajan myötä modulaarisen koodin käsite kielissä, kuten C:ssä, esitteli ajatuksen, että koodin pilkkominen yksiköihin (funktiot tai proseduurit) johtaa parempaan järjestelyyn ja selkeämpään logiikkaan.

JavaScriptissä meillä on tavallisten funktioiden lisäksi nuolifunktioita ES6:sta (2015) lähtien, jotka tarjoavat ytimekkäämmän syntaksin ja soveltuvat hyvin ei-menetelmäfunktioihin.

Vaihtoehdot ja parannukset JavaScript-koodin järjestämiseen sisältävät objektiiviset lähestymistavat käyttäen luokkia tai funktionaalisen ohjelmoinnin paradigmoja, jotka käsittelevät funktioita ensiluokkaisina jäseninä.

Toteutuksen kannalta JavaScriptin funktiot tukevat sulkeumia, tarjoten tavan säilyttää pääsy funktion toiminta-alueeseen suorituksen jälkeen, mikä on voimakasta kapseloinnissa ja tehdasfunktioiden luomisessa, muiden mallien joukossa.

## Katso myös
- MDN Web Docs funktioista: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Functions
- JavaScript-suunnittelumallit: https://addyosmani.com/resources/essentialjsdesignpatterns/book/
- Puhdaskoodi JavaScript: https://github.com/ryanmcdermott/clean-code-javascript
