---
title:                "TypeScript: Merkkijonon muuttaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Usein ohjelmoijilla on tarve muuttaa erilaisia merkkijonoja pienikirjaimiseksi. Tämä voi olla tarpeen esimerkiksi vertaillessa merkkijonoja tai validoidessa käyttäjän syöttöä.

## Ohjeet

### TypeScript ohjeilla

Merkkijonon muuttaminen pienikirjaimiseksi TypeScriptillä on erittäin helppoa. Voit käyttää siihen sisäänrakennettua `toLowerCase()`-metodia, joka muuttaa kaikki merkkijonon kirjaimet pienikirjaimisiksi.

```TypeScript
let merkkijono: string = "TÄMÄ ON KAIKKI SUURILLA KIRJAIMILLA";

let pienikirjaiminenMerkkijono: string = merkkijono.toLowerCase();

console.log(pienikirjaiminenMerkkijono);
// Output: "tämä on kaikki suurilla kirjaimilla"
```

### Tuloste

Yllä olevasta koodiesimerkistä näemme, miten alkuperäisestä merkkijonosta muodostui pienikirjaiminen versio `toLowerCase()`-metodin avulla. Tämän ansiosta voimme helposti vertailla merkkijonoja ilman huolta kirjainten koosta.

## Syvällinen tarkastelu

Kyse on tietysti enemmän kuin vain yksinkertaisesta `toLowerCase()`-metodin käytöstä. Tämä metodi perustuu Unicode-standardiin, joka kuvaa kaikki maailman merkit ja symbolit binäärikoodina. Tämä tarkoittaa, että tyypistä riippumatta TypeScript osaa käsitellä kaikenlaisia merkkejä ja muuttaa ne oikein pienikirjaimiseksi.

## Katso myös

- [TypeScriptin viralliset kotisivut](https://www.typescriptlang.org/)
- [Unicode-standardin dokumentaatio](https://unicode.org/standard/standard.html)
- [Merkkijonojen manipulointi TypeScriptissä](https://www.tutorialspoint.com/typescript/typescript_strings.htm)