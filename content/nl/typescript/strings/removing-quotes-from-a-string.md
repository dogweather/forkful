---
title:                "Quotes verwijderen uit een string"
aliases:
- /nl/typescript/removing-quotes-from-a-string/
date:                  2024-01-28T22:06:51.349323-07:00
model:                 gpt-4-0125-preview
simple_title:         "Quotes verwijderen uit een string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/typescript/removing-quotes-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het verwijderen van aanhalingstekens uit een tekenreeks betekent het weghalen van de omringende enkele (`'`) of dubbele (`"`) aanhalingstekens die tekenreeksliteralen in code definiëren. Programmeurs doen dit om verschillende redenen, zoals het formatteren van de uitvoer, het saniteren van gebruikersinvoer, of het voorbereiden van tekenreeksen voor parsing of opslag waarbij de aanhalingstekens onnodig zijn of fouten kunnen veroorzaken.

## Hoe te:
Hier is je nuchtere gids om die lastige aanhalingstekens los te snijden van je tekenreeksen in TypeScript.

```typescript
// Optie A: Enkele of dubbele aanhalingstekens vervangen met regex
function removeQuotes(input: string): string {
  return input.replace(/^["']|["']$/g, '');
}

console.log(removeQuotes(`"Gequoteerde tekenreeks"`)); // Gequoteerde tekenreeks
console.log(removeQuotes(`'Nog een'`)); // Nog een

// Optie B: Omgaan met tekenreeksen die beginnen en eindigen met verschillende aanhalingstekens
function removeMismatchedQuotes(input: string): string {
  return input.replace(/^(['"])(.*?)(?<!\1)\1$/, '$2');
}

console.log(removeMismatchedQuotes(`"Niet overeenkomend'`)); // "Niet overeenkomend'

// Optie C: Meerdere soorten aanhalingstekens verwijderen
function removeAllQuotes(input: string): string {
  return input.replace(/['"]+/g, '');
}

console.log(removeAllQuotes(`"'Mix'n'Match'"`)); // Mix'n'Match
```

## Diepere Duik
Lang voordat TypeScript zelfs maar een ding was, hadden JavaScript-coders al te maken met aanhalingstekentrucjes, en het verhaal is zo'n beetje hetzelfde voor TypeScript. Naarmate tijden veranderen, verandert ook de manier waarop we tekenreeksen opsnijden. Tegenwoordig, met de spierkracht van regex, schuiven we het gebruik van onhandige tekenreekslicing of andere vermoeiende methoden terzijde.

Hoewel de bovenstaande voorbeelden in de meeste van je behoeften zouden moeten voorzien, onthoud, het citeren kan complex worden. Genestelde, niet overeenkomende en geëscape'erde aanhalingstekens zijn de schavuiten die je willen laten struikelen. Voor deze gevallen heb je misschien meer geavanceerde patronen of zelfs parsers nodig om elke krullende zaak aan te kunnen. 

Alternatieven? Sommige mensen gaan voor bibliotheken zoals lodash, met methoden zoals `trim` en `trimStart` / `trimEnd`, die aangepast kunnen worden om aanhalingstekens te knippen als je de karakters instelt die je wilt knippen.

En voor jullie TypeScript-enthousiasten, laten we de typen niet vergeten. Hoewel we ons hier voornamelijk bezighouden met tekenreeksen, wanneer je werkt met gebruikersinvoer of parsing, kan het toevoegen van enkele typebewakers of zelfs generics helpen om je code net zo veilig te houden als je aanhalingstekens zijn getrimd.

## Zie Ook
Bekijk deze virtuele hotspots voor meer info:

- MDN Web Docs over regex (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- Officiële TypeScript Documentatie (https://www.typescriptlang.org/docs/)
- You Don't Need Lodash/Underscore – Tekenreeks Helpers (https://github.com/you-dont-need/You-Dont-Need-Lodash-Underscore#strings)
- Stack Overflow: Doorkruis de loopgraven waar talloze devs hebben gevochten met aanhalingstekenrampen (https://stackoverflow.com/)
