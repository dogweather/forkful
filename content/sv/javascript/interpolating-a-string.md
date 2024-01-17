---
title:                "Interpolering av en sträng"
html_title:           "Javascript: Interpolering av en sträng"
simple_title:         "Interpolering av en sträng"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Interpolering av en sträng är när du blandar variabler i din textsträng för att skapa en ny sträng. Det är ett vanligt verktyg för programmerare eftersom det gör det lättare att skapa dynamiskt och varierande innehåll baserat på variabler och användarinput.

## How to:
Detta görs genom att använda en specialteckenuppsättning, kallad template literals, som börjar och slutar med backtick (`) istället för citattecken (' eller "). Inuti dessa kan du skriva din sträng som vanligt, men använda variabler genom att placera dem inom uttryckslängor (\${}).

```Javascript
let namn = 'Lisa';
console.log(`Hej, mitt namn är ${namn}.`); // Output: Hej, mitt namn är Lisa.
```

Det är också möjligt att göra beräkningar eller köra funktioner inuti strängen genom att placera dem inom uttryckslängor.

```Javascript
let nummer1 = 5;
let nummer2 = 3;
console.log(`Summan av ${nummer1} och ${nummer2} är ${nummer1 + nummer2}.`); // Output: Summan av 5 och 3 är 8.
```

## Deep Dive:
Interpolering av strängar är inte ett nytt koncept, men det har blivit enklare och mer effektivt med template literals i den senaste versionen av Javascript. Innan dess användes metoden string concatenation, där man kombinerade strängar och variabler med hjälp av plustecken (+). Detta var ofta en krånglig och klumpig process, speciellt när man hanterade flera variabler samtidigt.

Det finns också alternativa sätt att interpolera strängar, till exempel genom att använda funktioner som "join" eller "replace". Men dessa metoder kan vara mer tidskrävande och kräva mer kod.

Implementeringen av template literals är relativt enkel och stöds nu av de flesta moderna webbläsare. Det är också möjligt att använda polyfills eller transpilering för att stödja äldre webbläsare.

## See Also:
För mer information, se dessa länkar:

- MDN Web Docs: [Template literals (strings)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- W3Schools: [Javascript String Interpolation](https://www.w3schools.com/js/js_string_interpolation.asp)
- Medium: [String Interpolation in JavaScript](https://medium.com/@satyareddy21/string-interpolation-in-javascript-1303328bb4a6)