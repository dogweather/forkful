---
title:                "Debug-tulosteen tulostaminen"
html_title:           "Bash: Debug-tulosteen tulostaminen"
simple_title:         "Debug-tulosteen tulostaminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Tulostaminen debug-ulostulo (tai debug-lokitus) on kehittäjän työkalu, jolla voidaan seurata ja analysoida ohjelmakoodin toimintaa sen ajon aikana. Kehittäjät käyttävät sitä järjestelmän tilan selvittämiseen, erityisesti kun kohtaavat odottamattomia ongelmia tai virheitä.

## Näin teet:
TypeScriptissä yleisin tapa tulostaa debug-ulostulo on käyttää `console.log()` -funktiota. Katsotaan esimerkki:

```TypeScript
let x: number = 5;
console.log("Value of x: " + x);
```
Tämä tulostaa konsoliin viestin: "Value of x: 5"

## Syvällisemmin
Debug-lokitus on ollut osa ohjelmointia sen alusta lähtien. Aikaisin looginen virheenkorjaus perustui valot merkkien tulostamiseen, jotka symboloivat koneen tilaa. Nykyaikana ohjelmat voidaan suunnitella lokitukseen erityisillä kirjastoilla, kuten `winston` ja `morgan` Node.js:ssä.

Tulostus on debug-toiminnon yksinkertaisin muoto, mutta muita vaihtoehtoja, kuten `console.debug()`, `console.info()`, `console.warn()`, ja `console.error()`, tarjoavat erilaisia tapoja tiedon esittämiseen.

TypeScriptin `console.log()` on alun perin JavaScriptistä, ja se on useimmin käytetty debug-lokitusmenetelmä. Se toimii kirjoittamalla annetut argumentit alla olevaan standardiin ulostuloon.

## Katso myös
Seuraavaa resurssit auttavat sinua ymmärtämään debug-ulostuloa paremmin: