---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Arduino: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Tekstin hakeminen ja korvaaminen tarkoittaa tekstijonon etsimistä luotussa ohjelmassa ja tämän jonon korvaamista toisella. Koodaajat tekevät näin, kun he haluavat muuttaa muuttujien, funktioiden tai muiden ohjelmointielementtien nimeä tai arvoa koko ohjelmassa.

## Näin se tehdään:
Löydä ja korvaa teksti TypeScriptissä `String.prototype.replace()` -metodilla. Metodi palauttaa uuden merkkijonon, jossa korvaus on suoritettu.

```TypeScript
let teksti = 'Tervetuloa TypeScriptiin!';
let uusiTeksti = teksti.replace('TypeScriptiin', 'JavaScriptiin');
console.log(uusiTeksti);
```
Output:
```
Tervetuloa JavaScriptiin!
```

## Syvempi sukellus
Tekstin hakeminen ja korvaaminen ei ole uusi konsepti, se on ollut olemassa alusta asti, kun ohjelmointikielet kehitettiin. TypeScriptissä tämä toteutetaan `String.prototype.replace()` -metodilla, mutta vanhemmissa ohjelmointikielissä, kuten C ja Perl, prosessi on monimutkaisempi.

Vaihtoehtoinen tapa tehdä samaa JavaScriptissä on käyttää regular expressions -lausekkeita. Ne tarjoavat paljon enemmän joustonvaraa ja tehokkuutta, mutta ovat myös monimutkaisempia.

```TypeScript
let teksti = 'Tervetuloa TypeScriptiin!';
let uusiTeksti = teksti.replace(/TypeScriptiin/i, 'JavaScriptiin');
console.log(uusiTeksti);
```

## Katso myös
- [Mozilla Developer Network: String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Regular expressions in JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [JavaScript replace method](https://www.w3schools.com/jsref/jsref_replace.asp)