---
date: 2024-01-20 17:42:46.102568-07:00
description: "Miten: JavaScriptin alkuaikoina merkkijonojen manipuloiminen oli yksinkertaisempaa.\
  \ Ajan my\xF6t\xE4 s\xE4\xE4nn\xF6lliset lausekkeet ovat tulleet tarpeellisiksi\u2026"
lastmod: '2024-04-05T21:53:58.512624-06:00'
model: gpt-4-1106-preview
summary: JavaScriptin alkuaikoina merkkijonojen manipuloiminen oli yksinkertaisempaa.
title: Merkkien poistaminen hakemalla osumia kaavaan
weight: 5
---

## Miten:
```javascript
let teksti = "H3i! M1t3n m3n33?";
let puhdistettuTeksti = teksti.replace(/\d/g, ''); // Poistetaan numerot
console.log(puhdistettuTeksti); // "Hei! Miten menee?"

let osoite = "käyttäjä@esimerkki.com";
let paikallinenOsa = osoite.split("@")[0];
let domain = osoite.split("@")[1].replace(/\.com$/, ''); // Poistetaan '.com' lopusta
console.log(`${paikallinenOsa}@${domain}`); // "käyttäjä@esimerkki"
```

## Syväluotaus:
JavaScriptin alkuaikoina merkkijonojen manipuloiminen oli yksinkertaisempaa. Ajan myötä säännölliset lausekkeet ovat tulleet tarpeellisiksi monimutkaisten tekstipuhdistustoimenpiteiden suorittamiseen. Vaihtoehtoina kaavojen poistoon voidaan käyttää myös muita string-metodeja, kuten `split`, `slice` ja `substring`, jos tarkoituksena on vain vakiomuotoisen merkkijonon käsittely. Implementointiyksityiskohdissa on otettava huomioon, että säännöllisten lausekkeiden tehokkuus ja muoto riippuvat kohdatusta datatyypistä ja sen rakenteesta.

## Katso Myös:
- MDN Web Docs, säännölliset lausekkeet: [MDN Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- JavaScript String metodeja: [MDN String Methods](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
