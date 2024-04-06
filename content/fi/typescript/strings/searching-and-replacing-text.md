---
date: 2024-01-20 17:58:57.089494-07:00
description: "How to: Tekstin hakeminen ja korvaaminen juontaa juurensa tekstink\xE4\
  sittelyn alkuh\xE4m\xE4riin, kun yksinkertaiset komentorivity\xF6kalut, kuten `sed`\
  \ Unix-\u2026"
lastmod: '2024-04-05T21:53:57.858424-06:00'
model: gpt-4-1106-preview
summary: "Tekstin hakeminen ja korvaaminen juontaa juurensa tekstink\xE4sittelyn alkuh\xE4\
  m\xE4riin, kun yksinkertaiset komentorivity\xF6kalut, kuten `sed` Unix-j\xE4rjestelmiss\xE4\
  , mahdollistivat sen."
title: Tekstin etsiminen ja korvaaminen
weight: 10
---

## How to:
```TypeScript
function replaceString(original: string, searchValue: string, replaceValue: string): string {
  return original.replace(new RegExp(searchValue, 'g'), replaceValue);
}

// Esimerkki käytöstä:
const story: string = "Old MacDonald had a farm, E-I-E-I-O.";
const updatedStory: string = replaceString(story, "Old", "Young");

console.log(updatedStory); // Tulostaa: Young MacDonald had a farm, E-I-E-I-O.
```

## Deep Dive
Tekstin hakeminen ja korvaaminen juontaa juurensa tekstinkäsittelyn alkuhämäriin, kun yksinkertaiset komentorivityökalut, kuten `sed` Unix-järjestelmissä, mahdollistivat sen. TypeScriptissä, kuten monissa moderneissa kielissä, `replace`-funktio käyttää säännöllisiä lausekkeita tekstinhakuun ja korjaukseen, mikä tekee siitä voimakkaan. Vaihtoehtoisesti voit käyttää kirjastoja, kuten `lodash` tai `replace-in-file`, jotka tarjoavat lisäominaisuuksia ja yksinkertaistettua APIa.

Hakuarvon ja korvattavan arvon kehittyneempää käsittelyä varten voit käyttää lamda-lausekkeita (arrow functions):

```TypeScript
const result: string = story.replace(/(Old|Young) MacDonald/, (match) => {
  return match === "Old MacDonald" ? "Young MacDonald" : "Old MacDonald";
});
```

Tässä otetaan käyttöön dynaamisesti muuttuva logiikka korvaukselle, riippuen vastaavuudesta. Tällainen joustavuus on hyödyllinen kompleksisissa korjaustarpeissa.

## See Also
- MDN RegExp - [developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- TypeScript Handbook - [typescriptlang.org/docs/handbook/intro.html](https://www.typescriptlang.org/docs/handbook/intro.html)
- `lodash` replace - [lodash.com/docs/#replace](https://lodash.com/docs/#replace)
- `replace-in-file` npm package - [npmjs.com/package/replace-in-file](https://www.npmjs.com/package/replace-in-file)
