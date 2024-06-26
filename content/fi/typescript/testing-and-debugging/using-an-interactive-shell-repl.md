---
date: 2024-01-26 04:18:38.178307-07:00
description: "Miten: TypeScript ei tule oman REPL:ns\xE4 kanssa. K\xE4ytet\xE4\xE4\
  n `ts-node`a, TypeScriptin suoritusymp\xE4rist\xF6\xE4 Node.js:lle, joka sis\xE4\
  lt\xE4\xE4 REPL:n. Asenna ensin\u2026"
lastmod: '2024-03-13T22:44:56.316997-06:00'
model: gpt-4-0125-preview
summary: "TypeScript ei tule oman REPL:ns\xE4 kanssa."
title: "Interaktiivisen komentotulkin (REPL) k\xE4ytt\xF6"
weight: 34
---

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
