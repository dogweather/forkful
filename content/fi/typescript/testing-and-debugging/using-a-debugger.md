---
title:                "Debuggerin käyttö"
aliases:
- /fi/typescript/using-a-debugger.md
date:                  2024-01-26T04:11:09.321612-07:00
model:                 gpt-4-0125-preview
simple_title:         "Debuggerin käyttö"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/using-a-debugger.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Debugger on työkalu, jonka avulla voit tutkia ja muuttaa koodisi sisäisiä toimintoja sen suorituksen aikana. Ohjelmoijat käyttävät sitä virheiden korjaamiseen koodinsa läpi kulkemalla, muuttujien tarkastelulla ja ohjelman kulun ymmärtämisellä.

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
