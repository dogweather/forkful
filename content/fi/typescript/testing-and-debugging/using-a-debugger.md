---
date: 2024-01-26 04:11:09.321612-07:00
description: "P\xE4\xE4st\xE4ksesi alkuun debuggerin kanssa TypeScriptiss\xE4 tarvitset\
  \ vain tuetun IDE:n (kuten Visual Studio Code) ja `launch.json`-konfiguraation.\
  \ T\xE4ss\xE4 on nopea\u2026"
lastmod: '2024-03-13T22:44:56.319854-06:00'
model: gpt-4-0125-preview
summary: "P\xE4\xE4st\xE4ksesi alkuun debuggerin kanssa TypeScriptiss\xE4 tarvitset\
  \ vain tuetun IDE:n (kuten Visual Studio Code) ja `launch."
title: "Debuggerin k\xE4ytt\xF6"
weight: 35
---

## Kuinka:
Päästäksesi alkuun debuggerin kanssa TypeScriptissä tarvitset vain tuetun IDE:n (kuten Visual Studio Code) ja `launch.json`-konfiguraation. Tässä on nopea esimerkki Node.js-sovellukselle:

```TypeScript
// app.ts
function greet(name: string) {
    console.log(`Hei, ${name}!`);
}

const userName = 'Ada';
greet(userName);
```

Jotta voisit debugata tämän, luo `launch.json` tiedosto `.vscode` kansioon:

```JSON
{
    "version": "0.2.0",
    "konfiguraatiot": [
        {
            "tyyppi": "node",
            "pyyntö": "launch",
            "nimi": "Käynnistä Ohjelma",
            "skipFiles": ["<node_internals>/**"],
            "ohjelma": "${workspaceFolder}/app.ts",
            "preLaunchTask": "tsc: build - tsconfig.json",
            "outFiles": ["${workspaceFolder}/build/**/*.js"]
        }
    ]
}
```

Aseta sitten keskeytyskohta `greet`-funktioosi napsauttamalla rivinumeron vasenta puolta IDE:ssäsi. Paina F5 aloittaaksesi debuggauksen, ja katso kuinka sovelluksesi pysähtyy keskeytyskohtaan. Voit nyt viedä hiiren muuttujien päälle, tarkkailla lausekkeita ja käydä koodisi läpi helposti.

## Syväsukellus
Aikana ennen kuin integroidut kehitysympäristöt (IDE:t) tulivat hienostuneiksi, debuggaus tehtiin usein print-lauseilla (tunnetaan myös nimellä `console.log` debuggaus). Se toimi, tavallaan, mutta oli kuin yrittäisi löytää neulan heinäsuovasta silmät peitettyinä.

Nykyajan debuggerit ovat kuin sveitsiläinen linkkuveitsi vianetsintään. TypeScriptin ja Node.js:n kehityksen myötä tarjolla on monenlaisia debuggereita, sisäänrakennetusta Node.js-tarkastajasta selaimen kehitystyökaluihin asiakaspuolen debuggausta varten.

Node.js-tarkastaja toimii liittämällä se käynnissä olevaan sovellukseesi; se kommunikoi Chrome DevTools -protokollan kautta, muuttaen Chrome-selaimesi mahtavaksi debuggauskonsoliksi. Tämä integraatio mahdollistaa visuaalisesti interaktiivisen ja yksityiskohtaisen debuggausistunnon, verrattuna perinteisiin komentorivin debuggauskäytäntöihin.

## Katso Myös
Lisälukemista ja ammattilaisvinkkejä varten, tsekkaa:

- [TypeScriptin Debuggaus Visual Studio Codessa](https://code.visualstudio.com/docs/typescript/typescript-debugging)
- [Node.js:n Debuggausopas](https://nodejs.org/en/docs/guides/debugging-getting-started/)
- [Chrome DevTools -dokumentaatio](https://developers.google.com/web/tools/chrome-devtools)
