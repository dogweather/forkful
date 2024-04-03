---
date: 2024-01-26 03:40:20.768954-07:00
description: "\xC5 fjerne anf\xF8rselstegn fra en streng betyr \xE5 bli kvitt de irriterende\
  \ anf\xF8rselstegnene som kan rote til koden din, spesielt n\xE5r du analyserer\
  \ data eller\u2026"
lastmod: '2024-03-13T22:44:41.172362-06:00'
model: gpt-4-0125-preview
summary: "\xC5 fjerne anf\xF8rselstegn fra en streng betyr \xE5 bli kvitt de irriterende\
  \ anf\xF8rselstegnene som kan rote til koden din, spesielt n\xE5r du analyserer\
  \ data eller konstruerer JSON-objekter."
title: "Fjerne anf\xF8rselstegn fra en streng"
weight: 9
---

## Hvordan:
Forestille deg at du har en streng som er innpakket i doble anførselstegn, som `"\"Hei, verden!\""` og du ønsker den rene, uten anførselstegn-teksten. Her er et raskt JavaScript-kodesnutt for å frigjøre strengen din fra de anførselstegns lenkene:

```javascript
let quotedString = "\"Hei, verden!\"";
let unquotedString = quotedString.replace(/^"|"$/g, '');
console.log(unquotedString); // Utdata: Hei, verden!
```

Og hvis du har å gjøre med enkle anførselstegn? Bare juster regex litt:

```javascript
let singleQuotedString = "'Hei, verden!'";
let unquotedString = singleQuotedString.replace(/^'|'$/g, '');
console.log(unquotedString); // Utdata: Hei, verden!
```

Eller hva om strengen din er en blanding av begge? Ingen problemer:

```javascript
let mixedQuotedString = "\"'Hei, verden!'\"";
let unquotedString = mixedQuotedString.replace(/^["']|["']$/g, '');
console.log(unquotedString); // Utdata: 'Hei, verden!'
```

## Dypdykk
Før JSON tok over, var det å unnslippe anførselstegn et ville vesten av omvendte skråstreker og triks. Tidlige programmeringsspråk spilte ikke alltid fint med anførselstegn, noe som betydde mye manuell strengmanipulasjon. Nå, med standardiserte dataformater, handler fjerning av anførselstegn ofte om å rydde opp inndata før de behandles som JSON eller lagrer tekst uten formateringskonflikter.

Alternativer til `.replace()`? Sikkert! Du kan splitte og sammenføye en streng på anførselstegn, bruke `slice` hvis du er sikker på anførselstegnenes posisjoner, eller til og med regex match for å trekke ut den nødvendige teksten. Alt avhenger av konteksten.

Men glem ikke om kanttilfellene: anførselstegn inne i anførselstegn, unescaped anførselstegn og internasjonale tegn. Tenk på strengen din som et potensielt minefelt av unntak, og trå forsiktig. Moderne JavaScript-motorer er optimalisert for å håndtere regex-operasjoner effektivt, så de er generelt sett på som go-to, men det er alltid verdt å sjekke ytelsen for tunge databehandlingsoppgaver.

## Se Også
Grav dypere inn i strengmanipulasjon og regex:

- Mozilla Developer Network om String.replace(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- Regex101 for å teste dine regex-mønstre: https://regex101.com/
- JSON.org for å forstå hvorfor vi har å gjøre med så mange anførselstegn i moderne webutvikling: http://json.org/
