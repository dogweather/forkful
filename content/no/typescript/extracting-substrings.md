---
title:                "Hente ut delstrenger"
html_title:           "TypeScript: Hente ut delstrenger"
simple_title:         "Hente ut delstrenger"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?
Extracting substrings er en prosess der utvalgte deler av en tekststreng blir isolert og hentet ut. Dette er en vanlig teknikk som brukes av programmere for å manipulere tekststrenger på en mer effektiv måte.

## Hvordan:
```TypeScript
//Eksempel 1:
let tekststreng = "Hei, dette er en tekststreng!";
let delStreng = tekststreng.substring(4,8);
console.log(delStreng);
// Output: "dett"

//Eksempel 2:
let navn = "Kari Olsen";
let etternavn = navn.substr(5);
console.log(etternavn);
// Output: "Olsen"
```

## Dypdykk:
Historisk sett har substringer blitt brukt til å søke i store datasett og håndtere komplekse tekstmanipulasjoner. Det finnes også alternative metoder for å hente ut deler av en tekststreng, som for eksempel regular expressions. I TypeScript er substring-metoden en del av standard String-prototypen og tar imot to parametere: startindeks og sluttindeks. Det finnes også en relatert metode kalt substr som tar imot kun startindeks og henter ut resten av strengen fra dette punktet.

## Se også:
- MDN Dokumentasjon om substring: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring
- MDN Dokumentasjon om substr: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substr