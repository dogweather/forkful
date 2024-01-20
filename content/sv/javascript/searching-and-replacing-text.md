---
title:                "Sökning och ersättning av text"
html_title:           "Arduino: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Sök och Ersätt Text i JavaScript

## Vad och Varför?
Sökning och ersättning av text är processen att identifiera en viss textsträng och byta ut den med en annan. Denna teknik är ofta nödvändig vid manipulering av data och formatering av text i programmeringsuppgifter.

## Hur man Gör:
För att söka och ersätta text i JavaScript, använder vi `string.replace()` funktionen. Här är ett exempel:

```Javascript
let text = 'Hej Världen!';
let sökText = 'Världen';
let ersättText = 'Sverige';
let nyText = text.replace(sökText, ersättText);
console.log(nyText);
```
När du kör den här koden, kommer utmatningen att bli: `Hej Sverige!`

## Djupdykning
Möjligheten att söka och ersätta text i programmeringsspråk har varit närvarande sedan tidiga versioner av språk som COBOL och Fortran. I JavaScript, `replace()` funktionen används inte bara för att ersätta en viss sträng, men kan också använda reguljära uttryck för att matcha och ersätta komplexa mönster.

Alternativ för textersättning i JavaScript inkluderar att använda `split()` och `join()` funktionerna tillsammans, men detta är ofta mer komplicerat än att bara använda `replace()`.

Notera att `replace()` funktionen i JavaScript inte ändrar den ursprungliga strängen. Istället skapar den och returnerar en ny sträng.

## Se även
För mer information om `replace()` och reguljära uttryck i JavaScript, se följande länkar:
- [JavaScript Replace() metoden från W3Schools](https://www.w3schools.com/jsref/jsref_replace.asp)