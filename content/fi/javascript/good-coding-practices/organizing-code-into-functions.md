---
title:                "Koodin järjestäminen funktioihin"
aliases:
- /fi/javascript/organizing-code-into-functions/
date:                  2024-01-26T01:10:42.365157-07:00
model:                 gpt-4-1106-preview
simple_title:         "Koodin järjestäminen funktioihin"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Koodin järjestäminen funktioihin pilkkoo tehtävät uudelleenkäytettäviksi osiksi, mikä tekee koodista siistimpää ja ylläpidettävämpää. Tämä vähentää toistoa, helpottaa testaamista ja parantaa luettavuutta.

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
