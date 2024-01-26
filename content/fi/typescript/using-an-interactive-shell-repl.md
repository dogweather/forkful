---
title:                "Interaktiivisen komentotulkin (REPL) käyttö"
date:                  2024-01-26T04:18:38.178307-07:00
model:                 gpt-4-0125-preview
simple_title:         "Interaktiivisen komentotulkin (REPL) käyttö"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Read-Eval-Print-Loop (REPL) on ohjelmointiympäristö, joka ottaa vastaan yksittäisiä käyttäjän syötteitä, suorittaa ne ja palauttaa tuloksen käyttäjälle. Ohjelmoijat käyttävät REPL:iä nopeasti kokeillakseen koodinpätkiä, debugatakseen ja oppiakseen uusia kielen ominaisuuksia ilman täyden sovelluksen luomisen vaivaa.

## Miten:
TypeScript ei tule oman REPL:nsä kanssa. Käytetään `ts-node`a, TypeScriptin suoritusympäristöä Node.js:lle, joka sisältää REPL:n.

Asenna ensin globaalisti:
```bash
npm install -g ts-node
```

Käynnistä REPL kirjoittamalla `ts-node` komentoriville:
```bash
ts-node
```

Tässä nopea pätkä kokeiltavaksi:
```TypeScript
> let message: string = 'Hei, REPL!';
> console.log(message);
Hei, REPL!
> 
```
Lopettaaksesi istunnon, paina `Ctrl+D`.

## Syväsukellus
Historiallisesti REPL:t olivat näkyviä Lispin kaltaisissa kielissä, mahdollistaen dynaamisen koodin arvioinnin. Konsepti on sittemmin levinnyt, tullen interaktiivisen koodauksen peruspilariksi monissa kielissä.

TypeScriptille `ts-node` ei ole ainoa vaihtoehto. Vaihtoehtoja sisältävät TypeScript Playgroundin käyttö web-selaimessa tai muiden Node.js-pohjaisten REPL:ien käyttö, jotka tukevat TypeScriptiä sopivilla liitännäisillä.

Toteutuksen osalta `ts-node` käyttää TypeScriptin kääntäjän API:a kääntämään koodia lennossa ennen sen suorittamista Node.js:llä. Tämä antaa välitöntä palautetta ja on erityisen hyödyllistä kokeiltaessa TypeScriptin uusimpia ominaisuuksia ilman asennusvaivaa.

Yksi asia muistettavana – vaikka REPL on hieno pikatesteille, se ei korvaa perinteisen, testattavan ja ylläpidettävän koodin kirjoittamista. Se on työkalu oppimiseen ja tutkimiseen, ei korvike asianmukaisille kehityskäytännöille.

## Katso myös
- [TypeScriptin virallinen verkkosivusto](https://www.typescriptlang.org/)
- [ts-node GitHubissa](https://github.com/TypeStrong/ts-node)
- [Node.js REPL Dokumentaatio](https://nodejs.org/api/repl.html)
- [TypeScript Playground](https://www.typescriptlang.org/play)