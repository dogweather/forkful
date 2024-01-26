---
title:                "Att göra en sträng versal"
html_title:           "Bash: Att göra en sträng versal"
simple_title:         "Att göra en sträng versal"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad och varför?
Att kapitalisera en sträng innebär att omvandla första bokstaven i varje ord till stor bokstav medan övriga blir små bokstäver. Programmerare gör det för att standardisera textdata, förbättra läsbarheten och uppfylla stilistiska krav.

## Hur gör man:
```TypeScript
function capitalizeString(str: string): string {
  return str.replace(/\b\w/g, (firstLetter) => firstLetter.toUpperCase()).toLowerCase();
}

const title = "här är en titel att kapitalisera";
const capitalizedTitle = capitalizeString(title);
console.log(capitalizedTitle); // Output: "Här Är En Titel Att Kapitalisera"
```

## Djupdykning:
En lång tid tillbaka, innan programmeringsspråken, var textformatering en manuell uppgift. Idag har vi automatiska funktioner, som `capitalizeString` i TypeScript, som sköter jobbet åt oss. Alternativt kan man använda RegExp-funktioner som i exemplet ovan eller externa bibliotek såsom Lodash med metoden `_.startCase()`, men den inbyggda funktionen är oftast smidigast. När det gäller implementation, se upp för speciella fall som förkortningar och icke-alfabetiska tecken som inte bör förändras.

## Se även:
- MDN Web Docs om `String.prototype.replace()`: [MDN - String.replace](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- Lodashs `startCase` metod: [Lodash - startCase](https://lodash.com/docs/4.17.15#startCase)
- TypeScript officiella dokumentation: [TypeScript Lang - Docs](https://www.typescriptlang.org/docs/)
