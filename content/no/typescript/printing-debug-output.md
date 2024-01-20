---
title:                "Utskrift av feilsøkingsresultat"
html_title:           "Arduino: Utskrift av feilsøkingsresultat"
simple_title:         "Utskrift av feilsøkingsresultat"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

---
## Hva & Hvorfor?
Utskrift av feilsøkingsinformasjon (debug output) gir oss en detaljert statusoppdatering av vår kjørende kode. Programmerere gjør dette for å forstå og løse problemer som kan oppstå under kjøretiden av programvaren.

## Hvordan gjøre det:

I TypeScript kan vi bruke `console.log()` metoden for å skrive ut feilsøkingsinformasjon. La oss lage en enkel funksjon for å demonstrere dette.

```TypeScript
function debugPrint(msg: string): void {
    console.log('DEBUG: ' + msg);
}

// Bruker funksjonen
debugPrint('Dette er en feilsøkingsbeskjed');
```
Utføringen av denne koden i en nettleser eller Node.js vil skrive ut følgende:
```Text
DEBUG: Dette er en feilsøkingsbeskjed
```

## Dypt dykk:

1. Historisk sammenheng: `console.log()`-funksjonen har vært en grunnleggende del av JavaScript (og dermed TypeScript) siden de første dagene av nettsurfing. Dette er et pålitelig verktøy når det gjelder å spore koden vår.

2. Alternativer: `console` har andre metoder også som `console.warn()`, `console.error()`, og `console.info()`, som kan brukes til å logge ulike typer beskjeder. Videre kan biblioteker som `debug` for Node.js eller `logger` for nestJS tilby flere funksjoner.

3. Implementeringsdetaljer: `console.log()` skriver til `stdout` i Node.js og til nettleserkonsollen i nettleserbaserte applikasjoner. Dette kan påvirkes av spesifikke innstillinger for logging i for eksempel din nettleserutviklerverktøy.

## Se også:
- [console | Node.js v17.2.0 Documentation](https://nodejs.org/api/console.html)
- [Console - Web APIs | MDN](https://developer.mozilla.org/en-US/docs/Web/API/Console)
- [TypeScript - Documentation](https://www.typescriptlang.org/docs/)