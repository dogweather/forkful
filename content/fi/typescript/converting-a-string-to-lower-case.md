---
title:    "TypeScript: Merkkijonon muuntaminen pieniksi kirjaimiksi"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

On usein tarpeen muuttaa merkkijono pieniksi kirjaimiksi erityyppisten tiedostojen, tietokantojen ja ohjelmien käsittelyyn. Tämän vuoksi on tärkeää ymmärtää, miten merkkijonon muotoilua voidaan muuttaa TypeScript-ohjelmoinnissa.

## Miten

Yksi tapa muuttaa merkkijonoa pieniksi kirjaimiksi TypeScriptillä on käyttää "toLowerCase()" -funktiota. Seuraava esimerkki näyttää, miten tämä tehdään:

```TypeScript
let teksti = "TÄMÄ ON MERKKIJONO";
let pienetKirjaimet = teksti.toLowerCase();

console.log(pienetKirjaimet);
```

Tulostus: "tämä on merkkijono"

Yllä olevassa koodissa luomme muuttuja nimeltä "teksti", jossa on isoja kirjaimia sisältävä merkkijono. Sitten käytämme "toLowerCase()" -funktiota luodaksemme uuden muuttujan nimeltä "pienetKirjaimet", joka sisältää muunnetun merkkijonon. Lopuksi tulostamme muutetun merkkijonon konsoliin.

## Syvällisempi tarkastelu

Merkkijonon muuttaminen pieniksi kirjaimiksi TypeScriptillä ei rajoitu vain "toLowerCase()" -funktioon. Myös "toLocaleLowerCase()" -funktio voi olla hyödyllinen, jos haluat huomioida erilaisia kieliasetuksia merkkijonossa. Lisäksi voit käyttää myös "toUpperCase()" ja "toLocaleUpperCase()" -funktioita muuntamaan merkkijonon isoihin kirjaimiin. On tärkeää huomata, että nämä funktiot eivät muuta alkuperäisen muuttujan arvoa, vaan ne luovat uuden muuttujan, joka sisältää muunnetun merkkijonon.

## Katso myös

- [TypeScript virallinen dokumentaatio](https://www.typescriptlang.org/docs)
- [Funktioita merkkijonojen käsittelyyn](https://www.geeksforgeeks.org/javascript-string-prototype-tolowercase-function/)
- [Merkkijonon muotoilu TypeScriptissä](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)