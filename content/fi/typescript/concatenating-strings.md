---
title:    "TypeScript: Merkkijonojen yhdistäminen"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Ohjelmoijat usein tarvitsevat yhdistää merkkijonoja jotta pystyvät luomaan monimutkaisempia tekstejä tai viestejä. Tämä on tärkeää myös silloin kun halutaan tulostaa tietoa käyttäjälle tai tallentaa se tietokantaan. TypeScriptin merkkijonojen yhdistämisominaisuus mahdollistaa tämän prosessin helposti.

## Miten

```TypeScript
const nimi = "Matti";
const tervehdys = "Hei " + nimi + ", tervetuloa sivustollemme!";
console.log(tervehdys);
```

Tässä esimerkissä käytämme "+" -operaattoria yhdistämään kaksi merkkijonoa yhdeksi. Lopputuloksena tulostuu "Hei Matti, tervetuloa sivustollemme!" konsoliin. Voimme myös yhdistää useita merkkijonoja yhtäaikaisesti.

```TypeScript
const kaupunki = "Helsinki";
const maa = "Suomi";
const yhteenveto = `Asun ${kaupunki}, joka sijaitsee maassa ${maa}.`;
console.log(yhteenveto);
```

Toisessa esimerkissä käytämme backtick-merkkejä (`) ja ${} -syntaksia luodaksemme yhden merkkijonon useiden muuttujien kanssa. Lopputuloksena tulostuu "Asun Helsinki, joka sijaitsee maassa Suomi." konsoliin.

## Syvällisempi sukellus

Stringien yhdistämisessä on tärkeää huomata, että TypeScript osaa automaattisesti muuntaa muut tyypit merkkijonoksi. Tämä tarkoittaa sitä, että voimme yhdistää myös numeroita ja boolean-arvoja merkkijonoihin ilman että tarvitsee erikseen muuntaa niitä. Esimerkiksi:

```TypeScript
const ikä = 30;
const tervetuloa = "Tervetuloa, olet " + ikä + " vuotta vanha!";

console.log(tervetuloa);
```

Tulostuu "Tervetuloa, olet 30 vuotta vanha!" konsoliin.

Voimme myös käyttää erilaisia operaattoreita osana merkkijonojen yhdistämistä.

```TypeScript
console.log("Tervetuloa sivustolle! " + "Jatka sivustomme selailua " + "saadaksesi lisää tietoa.");
```

Tämä tulostaa "Tervetuloa sivustolle! Jatka sivustomme selailua saadaksesi lisää tietoa." konsoliin.

## Katso myös

- [TypeScriptin virallinen dokumentaatio merkkijonojen muotoilusta](https://www.typescriptlang.org/docs/handbook/declaration-files/by-example.html#string-formatting)
- [W3Schools sivusto merkkijonojen yhdistämisestä Javascriptillä](https://www.w3schools.com/js/js_string_concat.asp)
- [Vuoropuhelun artikkeli stringien yhdistämisestä](https://dev.to/codementor/how-to-concatenate-strings-in-typescript-24c8)