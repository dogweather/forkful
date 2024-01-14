---
title:    "Javascript: Utskrift av feilsøkingsutdata"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Når man programmerer, er det viktig å kunne finne feil og problemer i koden raskt. Dette kan være spesielt vanskelig når man arbeider med større prosjekter og komplekse kodesnutter. Derfor er det nyttig å vite hvordan man printer ut debug output, eller feilutskrifter, for å kunne identifisere feil og finne løsninger på dem.

## Hvordan

I JavaScript kan man enkelt printe ut debug output ved å bruke funksjonen `console.log()`. Denne funksjonen tar inn en eller flere argumenter og skriver dem ut til konsollen. La oss si at vi vil printe ut verdien av en variabel ved navn `nummer`. Da kan vi gjøre det på følgende måte:

```Javascript
console.log(nummer);
```

Dette vil skrive ut verdien av variabelen til konsollen når koden blir kjørt. Om vi nå endrer verdien av `nummer`, vil konsollen skrive ut den nye verdien neste gang `console.log()` blir kjørt.

Man kan også printe ut tekst og variabler på samme tid ved å bruke såkalte "template literals". Dette gjøres ved å bruke backticks (`) istedenfor anførselstegn (") eller enkle anførselstegn (') i strengen. La oss se på et eksempel:

```Javascript
console.log(`Tallet er ${nummer}`);
```

Her vil teksten "Tallet er" alltid være den samme, men verdien av `nummer` vil variere.

## Deep Dive

I tillegg til `console.log()` finnes det flere andre funksjoner man kan bruke for å printe debug output, som for eksempel `console.table()` eller `console.error()`. Det finnes også metoder for å formatere utskriften på ulike måter, som for eksempel `console.group()` og `console.count()`.

Det er også mulig å bruke betingelser for å bare printe ut debug output hvis visse betingelser er oppfylt. Dette kan være nyttig for å redusere mengden av debug output som blir skrevet ut og for å unngå at konsollen blir overfylt med unødvendig informasjon.

Det er også verdt å nevne at det finnes ulike debugging-verktøy og utvidelser som kan være nyttige for å feilsøke og inspisere koden på en mer detaljert måte.

## Se også

* [MDN Web Docs](https://developer.mozilla.org/no/docs/Web/JavaScript) - documentasjon for JavaScript
* [Chrome DevTools](https://developers.google.com/web/tools/chrome-devtools) - debugging-verktøy for Google Chrome
* [VS Code](https://code.visualstudio.com/) - populær teksteditor med innebygd debugging-funksjonalitet