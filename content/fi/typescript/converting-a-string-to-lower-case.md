---
title:                "TypeScript: Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi: Merkkijonon muuntaminen pieniksi alkukirjaimiksi
Merkkijonojen muuntaminen pieniksi alkukirjaimiksi on hyödyllinen ohjelmointitekniikka, jota voidaan käyttää monissa eri tilanteissa. Se voi auttaa tekstin muokkauksessa tai tarkkuuden parantamisessa tietojenkäsittelyssä.

## Kuinka tehdä:
Voit muuntaa merkkijonon pieniksi alkukirjaimiksi TypeScriptillä käyttämällä toimintoa `toLowerCase()`. Tämä toiminto muuntaa kaikki merkkijonon isot kirjaimet vastaaviin pieniin kirjaimiin ja palauttaa uuden muokatun merkkijonon.
```
TypeScript const sana = "Tämä On Esimerkki";
console.log(sana.toLowerCase()); // tulostaa "tämä on esimerkki"
```

Voit myös käyttää `toLowerCase()` yhdessä muuttujien kanssa, jolloin voit muuntaa eri arvoja erikseen. Esimerkiksi:
```
TypeScript let etunimi = "JUHANI";
let sukunimi = "VIRTANEN";

console.log(etunimi.toLowerCase()); // tulostaa "juhani"
console.log(sukunimi.toLowerCase()); // tulostaa "virtanen"
```

## Syvempi sukellus:
`toLowerCase()` toiminto hyödyntää Unicode-merkkijonojen muuntamista kirjainten kokoeriin. Tämän ansiosta se voi käsitellä erilaisia kieliä ja erikoismerkkejä oikein. On myös tärkeää huomata, että `toLowerCase()` ei tee muutoksia alkuperäiseen merkkijonoon, vaan palauttaa uuden muokatun version.

## Katso myös:
- [Microsoftin virallinen TypeScript dokumentaatio](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-0.html)
- [MDN Web Docs - JavaScript-toiminto `toLowerCase()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)