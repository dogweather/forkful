---
title:                "Tulostusvirheiden palauttaminen"
html_title:           "TypeScript: Tulostusvirheiden palauttaminen"
simple_title:         "Tulostusvirheiden palauttaminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Joskus koodatessa voi tulla tarve selvittää tarkemmin, mitä ohjelma tekee ja miten se käsittelee eri tilanteita. Tässä tilanteessa voi olla hyödyllistä tulostaa debug-tietoa, joka auttaa hahmottamaan ohjelman suoritusta.

## Miten

```TypeScript
// Esimerkki debug-tekstin tulostamisesta
console.log("Tämä on debug-teksti.");
```

Debug-tekstiä voidaan tulostaa koodissa käyttämällä `console`-objektia ja sen `log`-metodia. Tämän avulla voidaan tulostaa haluttuja viestejä tai muuttujien arvoja ohjelman suorituksen aikana.

```TypeScript
// Tulostetaan muuttujan arvo
let i = 5;
console.log(`Muuttujan i arvo on ${i}`);
```

Debug-tekstin tulostaminen voi auttaa havaitsemaan mahdollisia virheitä koodissa ja helpottaa virheiden selvittämistä. Se voi myös olla hyödyllistä, kun halutaan seurata koodin suoritusta ja varmistaa, että ohjelma käsittelee eri tapauksia oikein.

## Syvemmälle

`console`-objektin lisäksi TypeScriptissä on myös `Debugger`-luokka, jonka avulla voidaan tulostaa tietoa vaihtehtisen debuggerin, kuten Chrome-kehittäjätyökalujen, kautta.

```TypeScript
// Tulostetaan numeroiden taulukko debuggerin kautta
let numbers = [1, 2, 3];
debugger;
```

Tämä avaa debuggerin, jossa voi tarkastella muuttujien arvoja ja suorittaa koodia vaiheittain. Tämä on hyödyllistä monimutkaisemman koodin debuggaamisessa ja mahdollisten bugin löytämisessä.

## Katso myös

- [TypeScriptin viralliset ohjeet debuggaamiseen](https://www.typescriptlang.org/docs/handbook/debugging.html)
- [Chrome DevToolsin käyttö TypeScriptin debuggaamiseen](https://www.hanselman.com/blog/TypeScript-and-Debugging-JavaScript-In-Chrome-DevTools-in-Eclipse-And-ASPNET-5).