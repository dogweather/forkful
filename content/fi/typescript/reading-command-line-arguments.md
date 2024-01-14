---
title:                "TypeScript: Komentoriviparametrien lukeminen"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Komentoriviparametrien lukeminen on tärkeä taito kaikille TypeScript-ohjelmoijille. Se mahdollistaa ulkoisten tietojen syöttämisen ohjelmalle suoraan komentoriviltä, mikä on erittäin hyödyllistä erilaisten sovellusten ja skriptien kehittämisessä.

## Miten

Komentoriviparametrien lukemiseen TypeScriptissä on olemassa valmiita työkaluja, kuten `yargs` ja `commander`. Alla on esimerkki yksinkertaisesta TypeScript-ohjelmasta, joka lukee komentoriviltä annettuja parametreja ja tulostaa ne konsoliin:

```TypeScript
import yargs from "yargs";

// Määritellään komentoriviltä hyväksytyt parametrit
const argv = yargs.options({
  nimi: { type: "string", demandOption: true, alias: "n" },
  ikä: { type: "number", demandOption: true, alias: "i" },
  osoite: { type: "string", demandOption: true, alias: "o" }
}).argv;

// Tulostetaan parametrit konsoliin
console.log("Hei, olen " + argv.nimi + ", " + argv.ikä + " vuotta vanha ja asun osoitteessa " + argv.osoite);
```

Kun suoritat tämän ohjelman komentoriviltä antamalla sille nimen, iän ja osoitteen, näet tuloksen konsolissa:

```
$ ts-node index.ts -n Johanna -i 25 -o Esimerkkikatu 1
Hei, olen Johanna, 25 vuotta vanha ja asun osoitteessa Esimerkkikatu 1
```

## Syvempi sukellus

Kun käytämme `yargs`-työkalua komentoriviparametrien lukemiseen, voimme myös määrittää parametrien lajin, pakollisuuden ja aliaksen lisäksi muita ominaisuuksia. Esimerkiksi voimme lisätä parametriin kuvauksen, joka näytetään käyttäjälle, jos hän pyytää apua komennolla `--help`. Voimme myös rajata parametrien arvoja tietylle välille tai muokata niitä ennen niiden tallentamista. `yargs`-työkalun dokumentaatiossa on lisätietoja näistä mahdollisuuksista.

## Katso myös

- [yargs dokumentaatio](https://www.npmjs.com/package/yargs)
- [commander dokumentaatio](https://www.npmjs.com/package/commander)
- [TypeScriptin opiskelu alusta alkaen](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes.html)