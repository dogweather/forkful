---
title:                "TypeScript: Tulostaminen debuggaustulosteita"
simple_title:         "Tulostaminen debuggaustulosteita"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi: Miksi tulostaa debug-asteikkoja TypeScriptillä?

Tulostus debug-asteikkoja on tärkeä osa kehittäjien työkaluja, jotka auttavat tunnistamaan ja korjaamaan ohjelmointivirheitä. Tulostus debug-asteikkoja voi parantaa ohjelman lopullista laatua ja auttaa kehittäjiä ymmärtämään ohjelmakoodin toimintaa paremmin.

## Miten: Esimerkkejä koodilohkojen ja tulostusvuodotyyppien käytöstä TypeScriptissä

Tulostuksen debug-lauselausuntojen käyttö TypeScriptissä on yksinkertaista. Ensimmäinen vaihe on asettaa debug-asteikon seurantatapa halutulle tulostusvuodon tyypille, kuten konsolille tai tiedostoon.

```TypeScript
// Asettaa debug-asteikkojen seurantatavan konsolille
console.log("Debug-asteikon seurantatapa: konsoli");
// Tulostusarvojen tyyppi debug-asteikolle on merkkijono
console.log(`Debug-asteikon seurantatyyppi: merkkijono`);
```

Voit myös tulostaa muuttujia ja niiden arvoja, jotka auttavat sinua ymmärtämään ohjelmakoodin suoritusta ja poimimaan ongelman esiin. Tässä esimerkissä tulostamme muuttujan "i" arvon sisältäneen for-silmukan tulostamisen lopussa.

```TypeScript
// Tulostaa "i" -muuttujan arvon sisällä olevan for-silmukan lopussa
for (let i = 0; i < 10; i++) {
  console.log(`Silmukka on nyt loopattu kerran ja "i" -arvo on: ${i}`);
}
```

## Syvempi sukellus: Tietoa debug-asteikoista

Tulostus debug-asteikoilla voi olla erilaisia tarkoituksia. Se voi auttaa tekemään ohjelmakoodin kehittämisestä ja testaamisesta helpompaa ja tehokkaampaa, erityisesti suurten ohjelmakoodien tapauksessa. Se auttaa myös identifioimaan ja korjaamaan ongelmia ohjelman suorituskyvyssä ja käytettävyydessä.

On myös tärkeää hallita ja järjestää tulostus debug-asteikkoja asianmukaisesti, jotta ohjelmoijat eivät törmäisi niiden kanssa liikaa tietoa kerralla. Tässä tapauksessa vähemmän on enemmän, ja on tärkeää valita huolellisesti, mitä ja miten paljon haluat tulostaa.

## Katso myös

- [TypeScript virallinen verkkosivusto](https://www.typescriptlang.org/)
- [Debug-asteikkojen tulostaminen Node.js:ssa](https://nodejs.org/api/console.html#console_console_log_data_args) 
- [Tulostus debug-asteikoja Visual Studio Code -ohjelmointiympäristössä](https://code.visualstudio.com/docs/editor/debugging)