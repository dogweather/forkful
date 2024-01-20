---
title:                "Aloittaminen uuden projektin"
html_title:           "C: Aloittaminen uuden projektin"
simple_title:         "Aloittaminen uuden projektin"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Uuden projektin aloittaminen on ohjelmiston tai sovelluksen kehittämisen ensimmäinen vaihe. Ohjelmoijat aloittavat uuden projektin kehittämällä uuden ratkaisun tai parantamalla olemassa olevaa sovellusta.

## Miten Tehdään:

Aloitetaan uusi Typescript-projekti seuraavilla komennolla:

```TypeScript
npm init -y
```

Tämä luo uuden `package.json` tiedoston. Sitten asennetaan tarvittavat TypeScript-moduulit komennolla:

```TypeScript
npm install typescript ts-node --save-dev
```

Tämän jälkeen luodaan `tsconfig.json` ja `index.ts` tiedostot:

```TypeScript
npx tsc --init 
echo console.log('Hei maailma!') > index.ts
```

Sitten, voit suorittaa TypeScript-koodin:

```TypeScript
npx ts-node index.ts
```

Näytölle tulostuu teksti "Hei maailma!".

## Syvempi Sukellus:

Uuden projektin aloittamisen käsite on kehittynyt  vuosien varrella. Alkuvaiheessa ohjelmoijat loivat usein tiedoston järjestelmän ja aloittivat kehittämisen tyhjältä pöydältä. Nykyään monissa ohjelmistokehyksissä on mukana työkaluja, kuten Yeoman, joka automatisoi projektin luomisen.

Vaihtoehto Typescriptille on JavaScript tai uudempi ECMAScript. Vaikka TypeScript on tiukemmin typed, jotkut kehittäjät pitävät JavaScriptin joustavuudesta ja yksinkertaisuudesta.

Uuden projektin aloittamisen toteuttaminen voi vaihdella ohjelmointikielen, käyttöjärjestelmän ja käytetyn työkalun mukaan. Jotkut kehittäjät käyttävät edistyneitä integroituja kehitysympäristöjä (IDE), kun taas toiset saattavat käyttää yksinkertaisempia tekstieditoria.

## Katso myös:

1. [TypeScriptin viralliset dokkarit](https://www.typescriptlang.org/docs/): Täydellinen opas TypeScriptin syntaksiin ja ominaisuuksiin.
2. [Node.js:n ja NPM:n asennus](https://nodejs.org/en/download/): Opas Node.js:n ja NPM:n asennukseen eri käyttöjärjestelmissä.
3. [TypeScript Deep Dive](https://basarat.gitbook.io/typescript/): Syvällinen katsaus TypeScriptiin. Kattava, ja hienosti kirjoitetu.