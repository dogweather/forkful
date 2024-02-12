---
title:                "Ta bort citattecken från en sträng"
aliases:
- /sv/typescript/removing-quotes-from-a-string/
date:                  2024-01-26T03:42:30.128781-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ta bort citattecken från en sträng"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ta bort citattecken från en sträng innebär att man tar bort de omgivande enkla (`'`) eller dubbla (`"`) citattecknen som definierar stränglitteraler i kod. Programmerare gör detta av flera anledningar, såsom att formatera utdata, sanera användarinmatning, eller förbereda strängar för tolkning eller lagring där citaten är onödiga eller kan orsaka fel.

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
