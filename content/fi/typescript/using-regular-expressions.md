---
title:                "Säännöllisten lausekkeiden käyttö"
date:                  2024-01-19
html_title:           "Bash: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
"Mitä ja Miksi?"
Käytämme säännöllisiä lausekkeita tekstin hakuun ja manipulointiin. Ne säästävät aikaa ja vaivaa kun tarvitsemme suodattaa, etsiä tai korvata tekstiä koodissa.

## How to:
"Kuinka:"
```TypeScript
// Etsitään sanoja, jotka alkavat 'p' kirjaimella.
let pattern = /\bp\w+/g;
let text = "programming in TypeScript is pretty fun";
let matches = text.match(pattern);
console.log(matches); // Tulostuu: ['programming', 'pretty']
```

```TypeScript
// Korvataan kaikki numerot tähdillä.
let digitPattern = /\d+/g;
let input = "Yhdeksänkymmentä9 kakkosta2";
let output = input.replace(digitPattern, '*');
console.log(output); // Tulostuu: "Yhdeksänkymmentä* kakkosta*"
```

## Deep Dive
"Sukellus syvyyksiin"
Säännölliset lausekkeet ovat syntyneet 1950-luvulla matemaatikko Stephen Kleenen töiden pohjalta. JavaScriptin säännölliset lausekkeet perustuvat Perl-kielen standardiin. Vaihtoehtoja säännöllisille lausekkeille voivat olla merkkijonojen sisäänrakennetut metodit, kuten `.indexOf` tai `.startsWith`, mutta ne eivät ole yhtä joustavia. TypeScript toteuttaa säännölliset lausekkeet pohjautuen JavaScriptin `RegExp`-objektiin.

## See Also
"Katso Myös"
- [MDN Web Docs: Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [RegExr: Learn, Build, & Test RegEx](https://regexr.com/)
- [TypeScript Handbook: Everyday Types](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#type-assertions)
