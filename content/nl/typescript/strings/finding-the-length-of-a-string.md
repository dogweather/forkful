---
title:                "De lengte van een string vinden"
aliases:
- /nl/typescript/finding-the-length-of-a-string.md
date:                  2024-01-28T22:00:39.923105-07:00
model:                 gpt-4-0125-preview
simple_title:         "De lengte van een string vinden"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/typescript/finding-the-length-of-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

De lengte van een string vinden betekent meten hoeveel tekens deze bevat. Programmeurs doen dit om invoer te valideren, door karakters te loopen of textweergave te beperken, onder andere redenen.

## Hoe te:

In TypeScript krijg je de lengte van een string met behulp van de `.length` eigenschap. Hier is een snel voorbeeld:

```typescript
let begroeting: string = "Hallo, TypeScript!";
console.log(begroeting.length); // Uitvoer: 18
```

Deze code verklaart een string variabele genaamd `begroeting` en logt vervolgens de lengte ervan in de console.

## Diepgaand

De `.length` eigenschap is een overblijfsel uit JavaScript, de voorouder van TypeScript. Het is een eenvoudige en universeel ondersteunde manier om de grootte van een string te krijgen.

Er zijn alternatieven, maar die compliceren de zaken meestal. Je zou bijvoorbeeld de string naar een array kunnen converteren en de elementen tellen:

```typescript
let begroetingArray: string[] = Array.from(begroeting);
console.log(begroetingArray.length); // Uitvoer: 18
```

Maar waarom de lange weg rondgaan? De `.length` eigenschap is efficiënt omdat strings onder de motorkap als karakterarrays worden opgeslagen, dus de lengte-informatie is direct beschikbaar.

Stel nu dat je te maken hebt met strings uit verschillende talen. Je zou problemen kunnen ondervinden met speciale karakters. De basis `.length` aanpak telt UTF-16 code-eenheden, wat problematisch kan zijn voor karakters die twee code-eenheden nodig hebben, bekend als surrogaatparen. In zulke gevallen kan de `.length` eigenschap je niet het aantal werkelijke karakters geven, ook bekend als codepunten.

Hier is hoe je strings kunt behandelen met surrogaatparen:

```typescript
function telCodePunten(str: string): number {
    return Array.from(str).length;
}

let fancyBegroeting: string = "Hallo, 🌍!";
console.log(telCodePunten(fancyBegroeting)); // Uitvoer: 9
```

Deze functie houdt rekening met de intriciteiten van stringencoding om ervoor te zorgen dat elk karakter, ongeacht of het een enkele of dubbele code-eenheid is, correct wordt geteld.

## Zie Ook

- De TypeScript Handleiding over Strings: [TypeScript Handleiding](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#strings)
- MDN Web Docs over de String length eigenschap: [String.prototype.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- Unicode en JavaScript: [JavaScript heeft een Unicode probleem - Mathias Bynens](https://mathiasbynens.be/notes/javascript-unicode)
