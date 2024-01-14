---
title:                "TypeScript: Å starte et nytt prosjekt"
programming_language: "TypeScript"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hvorfor

Å starte et nytt programmeringsprosjekt kan være en spennende utfordring for både erfarne og nye utviklere. Det gir muligheten til å utvikle nye ferdigheter, løse komplekse problemer og skape noe unikt. Med TypeScript som programmeringsspråk, kan du ta dine kodingsferdigheter til et nytt nivå og skape robuste og skalerbare applikasjoner.

## Hvordan

For å starte et TypeScript-prosjekt, må du først installere Node.js og TypeScript-kompilatoren på datamaskinen din. Deretter kan du følge disse enkle trinnene:

```
TypeScript --init
```

Dette vil lage en `tsconfig.json`-fil som lar deg konfigurere prosjektet ditt. Du kan spesifisere filbaner, kompileringsinnstillinger og annen informasjon i denne filen.

Neste steg er å opprette en `index.ts`-fil hvor du kan skrive TypeScript-kode. For eksempel:

```
let navn: string = "Ola";
console.log(`Hei ${navn}!`);
```

Nå kan du kompilere koden din ved å kjøre:

```
tsc index.ts
```

Dette vil generere en `index.js`-fil som du kan kjøre med Node.js:

```
node index.js
```

Konsollen skal nå vise "Hei Ola!" som output.

## Deep Dive

For å få mest mulig ut av ditt nye TypeScript-prosjekt, er det viktig å forstå noen grunnleggende konsepter, som typer og kompileringsfeil.

TypeScript er et typet språk, noe som betyr at du må angi typen til hver variabel og funksjon. Dette gir bedre kodekvalitet og færre feil, da TypeScript vil sjekke for eventuelle typemismatch før kjøring.

Hvis du gjør en feil i koden din og prøver å kompilere, vil TypeScript gi deg en kompileringsfeil som peker på hvor feilen oppstod og hva som må rettes. Dette gjør feilsøking mye enklere og bidrar til å utvikle mer pålitelige applikasjoner.

## Se også

- [TypeScript-dokumentasjonen](https://www.typescriptlang.org/docs/)
- [Node.js-nettstedet](https://nodejs.org/en/)
- [TypeScript: Opprinnelse og Bruk](https://medium.com/@tomsaleeba/typescript-origin-and-use-5e01b4e7ef3e)