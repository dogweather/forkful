---
date: 2024-01-26 03:42:33.095435-07:00
description: "\xC5 fjerne anf\xF8rselstegn fra en streng betyr \xE5 strippe bort de\
  \ omliggende enkle (`'`) eller doble (`\"`) anf\xF8rselstegnene som definerer strengliteraler\
  \ i\u2026"
lastmod: 2024-02-19 22:04:59.762055
model: gpt-4-0125-preview
summary: "\xC5 fjerne anf\xF8rselstegn fra en streng betyr \xE5 strippe bort de omliggende\
  \ enkle (`'`) eller doble (`\"`) anf\xF8rselstegnene som definerer strengliteraler\
  \ i\u2026"
title: "Fjerne anf\xF8rselstegn fra en streng"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å fjerne anførselstegn fra en streng betyr å strippe bort de omliggende enkle (`'`) eller doble (`"`) anførselstegnene som definerer strengliteraler i kode. Programmerere gjør dette av flere grunner, som for eksempel formatering av utdata, sanering av brukerinput, eller forberedelse av strenger for parsing eller lagring der anførselstegnene er unødvendige eller kan forårsake feil.

## Hvordan:
Her er din rett-på-sak guide for å kutte bort de irriterende anførselstegnene fra strengene dine i TypeScript.

```typescript
// Alternativ A: Erstatt enkle eller doble anførselstegn ved bruk av regex
function removeQuotes(input: string): string {
  return input.replace(/^["']|["']$/g, '');
}

console.log(removeQuotes(`"Quoted string"`)); // Quoted string
console.log(removeQuotes(`'Another one'`)); // Another one

// Alternativ B: Håndtere strenger som starter og slutter med forskjellige anførselstegn
function removeMismatchedQuotes(input: string): string {
  return input.replace(/^(['"])(.*?)(?<!\1)\1$/, '$2');
}

console.log(removeMismatchedQuotes(`"Mismatched'`)); // "Mismatched'

// Alternativ C: Fjerne flere typer anførselstegn
function removeAllQuotes(input: string): string {
  return input.replace(/['"]+/g, '');
}

console.log(removeAllQuotes(`"'Mix'n'Match'"`)); // Mix'n'Match
```

## Dypdykk
Langt tilbake før TypeScript var en greie, holdt JavaScript-koderne allerede på med anførselstegn-uenigheter, og historien er stort sett den samme for TypeScript. Ettersom tiden endrer seg, så endres også måten vi skjærer opp strenger på. Nå til dags, med regex sin muskelkraft, dytter vi til side bruk av klønete strengskjæring eller andre kjedelige metoder.

Selv om eksemplene ovenfor burde dekke de fleste av dine behov, husk, sitatbruk kan bli komplekst. Nøstede, mismatchede, og escapede anførselstegn er luringene som venter på å snuble deg opp. For disse kan det hende du trenger mer sofistikerte mønstre eller til og med parsers for å håndtere hver krøllete situasjon.

Alternativer? Noen folk liker å gå med biblioteker som lodash, med metoder som `trim` og `trimStart` / `trimEnd`, som kan skreddersys til å klippe anførselstegn hvis du setter karakterene du ønsker å snippe.

Og for dere TypeScript-entusiaster, la oss ikke glemme om typene. Selv om vi her stort sett har med strenger å gjøre, når du jobber med brukerinput eller parsing, å kaste inn noen typevoktere eller til og med generikk kan hjelpe sikre at koden din er like sikker som dine anførselstegn er trimmet.

## Se Også
Sjekk ut disse virtuelle hotspotene for mer info:

- MDN Web Docs om regex (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- Offisiell TypeScript Dokumentasjon (https://www.typescriptlang.org/docs/)
- Du Trenger Ikke Lodash/Underscore – Streng Hjelpere (https://github.com/you-dont-need/You-Dont-Need-Lodash-Underscore#strings)
- Stack Overflow: Traverser skyttergravene der utallige utviklere har bekjempet anførselstegnkatastrofer (https://stackoverflow.com/)
