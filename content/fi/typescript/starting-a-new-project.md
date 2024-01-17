---
title:                "Aloittamassa uutta projektia"
html_title:           "TypeScript: Aloittamassa uutta projektia"
simple_title:         "Aloittamassa uutta projektia"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

### Mitä ja miksi?
Uuden projektin aloittaminen tarkoittaa uuden ohjelmasovelluksen tai verkkosivuston kehittämistä. Ohjelmoijat tekevät sitä parantaakseen nykyisiä sovelluksia tai luodakseen täysin uuden. Se on tärkeä vaihe ohjelmistokehityksessä, koska se auttaa organisoimaan työtä ja varmistaa, että uusi projekti aloitetaan puhtaalta pöydältä.

### Kuinka:
```TypeScript
npm init
```
Käytä komentoa "npm init" aloittaaksesi uuden projektin TypeScriptilla. Tämä komento luo package.json-tiedoston, joka sisältää kaikki projektin riippuvuudet ja asetukset.

```TypeScript
npm install typescript --save-dev
```
Seuraavaksi tarvitset Typescriptin asentamisen. Tämä tehdään käyttämällä komentoa "npm install typescript --save-dev". Tämä asentaa Typescriptin projektin kehittäjän riippuvuuksiin.

```TypeScript
tsc --init
```
Nyt asennuksen jälkeen sinun täytyy alustaa uusi Typescript-tiedosto. Voit tehdä tämän komennolla "tsc --init". Tämä luo tsconfig.json-tiedoston, joka sisältää Typescript-kääntäjäohjeet.

### Syvemmälle:
Luotuaan uuden projektin, on hyvä miettiä erilaisia vaihtoehtoja. Typescript on yksi monista ohjelmointikielistä, jotka voit valita projektisi perustaksi. Muita suosittuja vaihtoehtoja ovat JavaScript, Python ja Java.

Kun lähdet rakentamaan uutta projektia, on myös hyvä pitää mielessä projektin skaalautumiskyky. Jos kyseessä on suurempi projekti, kannattaa harkita frameworkin käyttöä. Joitakin suosituimpia TypeScript-frameworkkeja ovat Angular ja React.

Kun aloitat uuden projektin, on myös tärkeää valita sopiva tekstieditori tai kehitysympäristö. Monet käyttävät Visual Studio Codea tai WebStormia, mutta valinta riippuu henkilökohtaisista mieltymyksistä ja projektin vaatimuksista.

### Katso myös:
- [TypeScriptin viralliset sivut](https://www.typescriptlang.org/)
- [Visual Studio Code](https://code.visualstudio.com/)
- [WebStorm](https://www.jetbrains.com/webstorm/)