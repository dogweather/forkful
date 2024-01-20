---
title:                "Interpolera en sträng"
html_title:           "C++: Interpolera en sträng"
simple_title:         "Interpolera en sträng"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Interpolering av en sträng i JavaScript är processen att införa variabler direkt in i en strängtext. Programmerare gör detta för att göra sin kod renare och lättare att läsa, vilket sparar tid och energi.

## Så här gör du:
Här är en grundläggande kodsnutt för interpolering av strängar i JavaScript:
```Javascript 
let namn = 'Karl';
let hälsning = `Hej, ${namn}!`;
console.log(hälsning); //"Hej, Karl!"
```
I det här exemplet skapar vi en sträng, "hälsning", där variabeln "namn" är infogad direkt i strängen med hjälp av '${...}' syntaxen.

## Djupt Dyk
Historiskt sett hade JavaScript inte inbyggd stränginterpolering förrän ES6 (ES2015). Innan det var konkatenation med plus-tecken (+) standardmetoden för att infoga variabler i strängar, vilket kan vara rörigt och svårläst med stora strängar och många variabler.

Alternativt kan du använda den äldre metoden för konkatenation om du arbetar i en äldre kodbas eller har kompatibilitetsproblem. Men för den bästa användarupplevelsen och korrekt kodstruktur bör du använda stränginterpolering. 

När det gäller implementering fungerar stränginterpolering i JavaScript genom tokenisering. JavaScript-motorn hittar '${...}' i en sträng och byter ut det med värdet av variabeln eller uttrycket inom parenteserna.

## Se Även 
För mer information och djupgående exempel, kolla in dessa källor:

- [MDN Web Docs: Template literals (Template literals (Template Strings))](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- [W3schools: JavaScript String Interpolation (JavaScript stränginterpolering)](https://www.w3schools.com/js/js_string_templates.asp)
- [JavaScript.info: Template literals](https://javascript.info/strings#tagged-templates)