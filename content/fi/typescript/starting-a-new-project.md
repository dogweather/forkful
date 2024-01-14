---
title:                "TypeScript: Uuden projektin aloittaminen"
programming_language: "TypeScript"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Miksi

Aloittamaan uuden projektin on yksi jännittävimmistä osista ohjelmistokehitystä. Se tarjoaa mahdollisuuden luoda jotain uutta ja mahdollistaa korkean luomisen tunteen. Uuden projektin aloittamisessa on myös hyödyllistä oppia uusia taitoja ja parantaa työkalupakettia.

## Kuinka

Uuden projektin aloittaminen TypeScriptillä on yksinkertaista ja se tarjoaa runsaasti ominaisuuksia parantaaksesi kehitysprosessiasi. Tässä on muutama esimerkki koodista ja tulostuksesta käyttäen markdown-koodia ``` TypeScript ... ```

```TypeScript
console.log("Hei maailma");
```

Tulostus: Hei maailma

```TypeScript
interface Henkilö {
  etunimi: string;
  sukunimi: string;
  ikä: number;
}

let henkilö: Henkilö = {
  etunimi: "Matti",
  sukunimi: "Meikäläinen",
  ikä: 25
};

console.log("Hei, olen " + henkilö.etunimi + " " + henkilö.sukunimi + " ja olen " + henkilö.ikä + " vuotta vanha.");
```

Tulostus: Hei, olen Matti Meikäläinen ja olen 25 vuotta vanha.

Koodiesimerkit ja -tulosteet auttavat sinua kokeilemaan ja oppimaan perusteet TypeScriptistä. Opi lisää erilaisista käsitteistä, kuten muuttujista, funktioista ja ehdoista, kehittääksesi taitojasi ja luodaksesi monimutkaisempia sovelluksia.

## Syväluotaus

Ennen uuden projektin aloittamista on tärkeää käyttää aikaa suunnitellaksesi tarvittavat vaiheet ja hahmottaaksesi projektin tavoitteet. Voit myös päättää, mihin kehitysympäristöön ja työkaluihin haluat keskittyä. TypeScriptin avulla voit luoda sekä frontend- että backend-ratkaisuja, joten voit valita alustan, joka sopii projektillesi parhaiten.

Lisäksi on hyödyllistä tehdä tutkimusta TypeScript-yhteisöstä ja löytää online-resursseja, jotka voivat auttaa sinua projektisi kehittämisessä. Muista myös dokumentoida koodiasi ja noudattaa parhaita käytäntöjä, jotta projekti pysyy järjestäytyneenä ja helppokäyttöisenä.

## Katso myös

- [TypeScriptin virallinen verkkosivusto](https://www.typescriptlang.org/)
- [TypeScript-koodiesimerkit ja oppitunnit](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes.html)
- [TypeScript-yhteisöfoorumi](https://github.com/microsoft/TypeScript/issues)
- [TypeScript-opetusohjelmat ja vinkit](https://github.com/accessToken19/typescript-tips-tricks-cheatsheet/blob/master/README-fi.md)