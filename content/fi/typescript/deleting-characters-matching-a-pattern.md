---
title:                "Merkkien poistaminen hakemalla osumia kaavaan"
aliases:
- fi/typescript/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:43:24.689964-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkien poistaminen hakemalla osumia kaavaan"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?
Mitä ja miksi? Kun poistetaan merkkejä, jotka vastaavat tiettyä kaavaa, siivotaan merkkijonoja. Tämä auttaa meitä pääsemään eroon ei-toivotuista merkeistä, kuten ylimääräisistä välilyönneistä tai erikoismerkeistä, järkeistämään syötettä tai valmistelemaan dataa käsittelyyn.

## How to:
Käytännössä:
```TypeScript
function removePatternFromString(input: string, pattern: RegExp): string {
  return input.replace(pattern, '');
}

// Esimerkki: Poistetaan kaikki numerot merkkijonosta
const originalString = 'Hei! Tämä on esimerkki123.';
const noNumbers = removePatternFromString(originalString, /\d+/g);
console.log(noNumbers); // Hei! Tämä on esimerkki.
```

```TypeScript
// Esimerkki: Poistetaan erikoismerkit, paitsi pisteet ja kysymysmerkit
const stringWithSpecialChars = 'Hei! Onko kaikki hyvin??? $$$';
const cleanedString = removePatternFromString(stringWithSpecialChars, /[^a-zA-ZäöåÄÖÅ .?]/g);
console.log(cleanedString); // Hei! Onko kaikki hyvin??? 
```

## Deep Dive:
Syväsukellus: Mallien mukainen merkkien poistaminen merkkijonoista ei ole uusi idea; se on ollut osa ohjelmoinnin perustyökalupakkia jo regexin (säännölliset lausekkeet) varhaispäivistä lähtien. TypeScriptissa, kuten JavaScriptissä, regexiä käytetään mallien tunnistamiseen ja manipulaatioon. Vaihtoehtoja on toki olemassa: voit käyttää yksinkertaisempia string-menetelmiä kuten `split` ja `join` tai loopata merkkijonon läpi poistaen merkkejä yksi kerrallaan, mutta regex tarjoaa suoraviivaisen ja tehokkaan tavan suorittaa monimutkaisia haku- ja korvaustoimenpiteitä. Käytettäessä TypeScriptiä, tyyppiturvallisuus auttaa varmistamaan, että funktiot käsittelevät odotetun tyyppisiä merkkijonoja ja regex-malleja, vähentäen virheiden mahdollisuutta.

## See Also:
Lisätietoja:
- [MDN Web Docs: Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html)
- [Regex Tester - Testaa säännölliset lausekkeet verkossa](https://regexr.com/)
