---
title:                "Tekstin etsiminen ja korvaaminen"
aliases: - /fi/typescript/searching-and-replacing-text.md
date:                  2024-01-20T17:58:57.089494-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tekstin etsiminen ja korvaaminen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
Tekstin hakeminen ja korvaaminen tarkoittaa merkkijonojen etsimistä ja niiden muuttamista toisiksi merkkijonoiksi. Ohjelmoijat käyttävät sitä datan puhdistukseen, muodon muuttamiseen ja automatisoituun tiedon korjaukseen.

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
