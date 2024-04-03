---
date: 2024-01-26 03:42:30.128781-07:00
description: "Hur man g\xF6r: H\xE4r \xE4r din rak-p\xE5-sak-guide f\xF6r att klippa\
  \ loss de irriterande citattecknen fr\xE5n dina str\xE4ngar i TypeScript."
lastmod: '2024-03-13T22:44:37.644301-06:00'
model: gpt-4-0125-preview
summary: "H\xE4r \xE4r din rak-p\xE5-sak-guide f\xF6r att klippa loss de irriterande\
  \ citattecknen fr\xE5n dina str\xE4ngar i TypeScript."
title: "Ta bort citattecken fr\xE5n en str\xE4ng"
weight: 9
---

## Hur man gör:
Här är din rak-på-sak-guide för att klippa loss de irriterande citattecknen från dina strängar i TypeScript.

```typescript
// Alternativ A: Ersätt enkla eller dubbla citat med regex
function removeQuotes(input: string): string {
  return input.replace(/^["']|["']$/g, '');
}

console.log(removeQuotes(`"Citerad sträng"`)); // Citerad sträng
console.log(removeQuotes(`'En till'`)); // En till

// Alternativ B: Hantera strängar som börjar och slutar med olika citat
function removeMismatchedQuotes(input: string): string {
  return input.replace(/^(['"])(.*?)(?<!\1)\1$/, '$2');
}

console.log(removeMismatchedQuotes(`"Omatchade'`)); // "Omatchade'

// Alternativ C: Trimma flera typer av citat
function removeAllQuotes(input: string): string {
  return input.replace(/['"]+/g, '');
}

console.log(removeAllQuotes(`"'Mix'n'Match'"`)); // Mix'n'Match
```

## Fördjupning
Långt innan TypeScript ens var en sak, hade JavaScript-kodare redan att göra med citatteckenkonster, och historien är mer eller mindre densamma för TypeScript. Med tiden förändras sättet vi skär upp strängar på. Nuförtiden, med regex muskelkraft, skjuter vi åt sidan att använda klumpig strängskärning eller andra tråkiga metoder.

Även om ovanstående exempel borde täcka de flesta av dina behov, kom ihåg, citattecken kan bli komplexa. Nästlade, omatchade och escapede (bakslas-skyddade) citattecken är de tricksters som väntar på att fälla dig. För dessa kan du behöva mer sofistikerade mönster eller till och med tolkare för att hantera varje krångelfall.

Alternativ? Vissa gillar att använda bibliotek som lodash, med metoder som `trim` och `trimStart` / `trimEnd`, som kan skräddarsys för att klippa citat om du anger de tecken du vill snippa bort.

Och för dig TypeScript-entusiaster, låt oss inte glömma typerna. Medan vi här främst har att göra med strängar, när du arbetar med användarinmatning eller tolkning, kan införandet av några typskydd eller till och med generiska typer hjälpa till att säkerställa att du håller din kod lika säker som dina citattecken är trimmade.

## Se även
Kolla in dessa virtuella heta platser för mer info:

- MDN Web Docs om regex (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- TypeScript Officiella dokumentationen (https://www.typescriptlang.org/docs/)
- You Don't Need Lodash/Underscore – Stränghjälpmedel (https://github.com/you-dont-need/You-Dont-Need-Lodash-Underscore#strings)
- Stack Overflow: Utforska skyttegravarna där otaliga utvecklare har stridit mot citatteckenkatastrofer (https://stackoverflow.com/)
