---
title:                "Merkkijonon interpolointi"
html_title:           "Bash: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Miten merkkijonojen interpolointia käytetään TypeScriptin avulla?
Tervetuloa oppimaan yhdessä merkkijonon interpoloinnista TypeScriptin (nykyversio) avulla yksinkertaisin, suoraviivaisin sanankääntein. Vältämme turhaa jaarittelua ja keskitymme asiaan. 

## Mikä & Miksi?
Merkkijonon interpolointi on tapa yhdistää muuttujia ja laskentaa suoraan merkkijonojen sisään. Tämä tekee koodin selkeämmäksi ja helpommin luettavaksi.

## Näin se toimii:
```TypeScript
let nimi = "Pekka";
let tervehdys = `Hei, ${nimi}!`;

console.log(tervehdys); // tulostaa: "Hei, Pekka!"
```

## Sukellus syvemmälle: 
Merkkijonon interpolointi on ollut käytössä vuosikymmenten ajan eri kielissä, mutta se lisättiin JavaScriptiin (ja siten TypeScriptiin) vasta ES6-versiossa. 

Vaihtoehtoja interpoloinnille ovat muun muassa perinteinen merkkijonojen yhdistäminen sekä + -operaattori. Niiden käyttö voi kuitenkin olla monimutkaista ja altis virheille.

Interpoloinnin taustalla TypeScript luo uuden merkkijonon, johon se liittää välittömästi muuttujat ja lausekkeet. Sen ansiosta koodista tulee siistimpää.

## Katso myös:
- [Merkkijonojen interpolointi MDN Web Docsissa](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Template_literals)
- [TypeScriptin viralliset dokumentit](https://www.typescriptlang.org/docs/)

Muista yhtenäinen ja siisti koodaus!