---
title:                "Komentoriviparametrien lukeminen"
html_title:           "TypeScript: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Miksi

On olemassa monia tilanteita, joissa on hyödyllistä lukea käyttöliittymän argumentteja TypeScriptissä. Esimerkiksi kun haluat muokata ohjelman toimintaa tiettyjen parametrien perusteella.

# Kuinka

Käyttöliittymän argumenttien lukeminen TypeScriptissä on erittäin helppoa. Sinun tarvitsee vain käyttää `process.argv` muuttujaa, joka sisältää kaikki annetut argumentit.

```TypeScript
// Käytetään splice-funktiota ottamaan ensimmäinen argumentti pois listalta (koska se sisältää tiedoston nimen)
const argumentit = process.argv.splice(2);

console.log(argumentit);
```

Esimerkiksi, jos ajatellut tiedostoa `node hello.ts arg1 arg2`, `argumentit` -muuttuja sisältää nyt `[arg1, arg2]`.

## Syvällisempi katsaus

`process.argv` sisältää todellisen argumentin lisäksi myös 2 lisäargumenttia: node-komentorivin ja tiedoston nimen. Tämä on tärkeää muistaa, kun käsittelet argumentteja TypeScriptissä. Muista myös, että `process.argv` palauttaa aina merkkijonot, joten sinun täytyy muuttaa ne numeroiksi tai muiksi tyypeiksi jos tarvitset niitä.

# Katso myös

- [Node.js -process.argv](https://nodejs.org/api/process.html#process_process_argv)
- [Reading command line arguments in TypeScript](https://stackoverrun.com/fi/q/1775988)
- [Using command line arguments in TypeScript](https://medium.com/@xavierafilip/using-command-line-arguments-in-typescript-18fe6f7f25b3)