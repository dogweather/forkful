---
title:                "Merkkien poistaminen hakemalla osumia kaavaan"
date:                  2024-01-20T17:42:46.102568-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkien poistaminen hakemalla osumia kaavaan"

category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
JavaScriptissa merkkien poistaminen tietyllä kaavalla tarkoittaa ei-toivottujen merkkien tai merkkijonojen karsimista tekstistä käyttämällä säännöllisiä lausekkeita tai metodeja. Ohjelmoijat tekevät tämän siistiäkseen dataa, kuten käyttäjän syötettä tai tiedostosta luetun tekstin.

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
