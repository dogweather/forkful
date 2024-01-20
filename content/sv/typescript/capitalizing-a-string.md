---
title:                "Gör en sträng versal"
html_title:           "TypeScript: Gör en sträng versal"
simple_title:         "Gör en sträng versal"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

---

# "String" Stor Bokstav i TypeScript

## Vad & Varför?

Att göra första bokstaven stor (capitalizing a string) betyder helt enkelt att omvandla den första bokstaven i en textsträng från små till stora bokstäver. Som programmerare gör vi detta för att förbättra läsbarheten eller för att uppfylla specifika programmeringsbehov.

## Hur man:

Här är ett exempel på hur du gör detta i TypeScript. 

```typescript
function capitalizeFirstLetter(str: string) {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

let myStr = 'hej världen';
myStr = capitalizeFirstLetter(myStr);

console.log(myStr);
```

Och detta kommer att skriva ut:

```
Hej världen
```
## Djupdykning

1. Historisk kontext: Denna funktion har historiskt varit del av många programmeringsspråk, vilket underlättar textmanipulation för programmerare.
  
2. Alternativ: Ett annat gängse sätt att göra det första tecknet stort i string i TypeScript är att använda `substring()` istället för `slice()`.

```typescript
function capitalizeFirstLetter(str: string) {
  return str.charAt(0).toUpperCase() + str.substring(1);
}
```

3. Implementeringsdetaljer: Funktionen `toUpperCase()` konverterar alla bokstäver i en sträng till versaler, men genom att använda `charAt(0)` fokuserar vi bara på det första tecknet. `slice(1)` och `substring(1)` används för att hämta resten av strängen utan att ändra det.

## Se också

- [MDN dokumentation](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase) för `toUpperCase()`, `charAt()`, `slice()` och `substring()`.
- [Stack Overflow konversation](https://stackoverflow.com/questions/1026069/how-do-i-make-the-first-letter-of-a-string-uppercase-in-javascript) om att göra första bokstaven stor i JavaScript.