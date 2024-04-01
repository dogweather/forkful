---
date: 2024-01-26 03:50:03.182181-07:00
description: "\xC5 bruke en feils\xF8ker betyr \xE5 tappe inn i spesialiserte verkt\xF8\
  y som lar deg titte under panseret p\xE5 koden din, og se den kj\xF8re steg for\
  \ steg. Programmerere\u2026"
lastmod: '2024-03-13T22:44:41.188106-06:00'
model: gpt-4-0125-preview
summary: "\xC5 bruke en feils\xF8ker betyr \xE5 tappe inn i spesialiserte verkt\xF8\
  y som lar deg titte under panseret p\xE5 koden din, og se den kj\xF8re steg for\
  \ steg. Programmerere\u2026"
title: "\xC5 bruke en feils\xF8ker"
---

## Hvordan:
Her er et stykke JavaScript-kode som ikke oppfører seg som forventet:

```javascript
function buggyMultiply(a, b) {
    return a + b; // Oops! Dette skal være en multiplikasjon, ikke addisjon.
}

let result = buggyMultiply(5, 3);
console.log('Resultat:', result);
```

Utskriften er feil:
```
Resultat: 8
```

La oss feilsøke i Chrome DevTools:

1. Åpne denne JS-en i en nettleser.
2. Høyreklikk og velg "Inspiser" for å åpne DevTools.
3. Klikk på "Sources"-fanen.
4. Finn din kodesnutt eller side og sett et brytepunkt ved å klikke på linjenummeret ved siden av `return`-påstanden.
5. Oppdater siden for å utløse brytepunktet.
6. Sjekk "Scope"-panelet for å se lokale variabler `a` og `b`.
7. Gå gjennom med "Gå over neste funksjonskall"-knappen.
8. Oppdag feilen i `return`-påstanden.
9. Fiks koden:
```javascript
function buggyMultiply(a, b) {
    return a * b; // Fikset!
}

let result = buggyMultiply(5, 3);
console.log('Resultat:', result);
```

Den korrigerte utskriften:
```
Resultat: 15
```

## Dypdykk
Konseptet med feilsøking har vært rundt siden de tidlige dagene av databehandling—legenden sier det startet da en møll ble funnet i en datamaskin på 1940-tallet! I dag tilbyr JavaScript-feilsøkere som innebygde nettleserverktøy (Chrome DevTools, Firefox Developer Tools) eller IDE-integrerte feilsøkere (Visual Studio Code, WebStorm) en mengde funksjoner.

Alternativer til innebygde feilsøkere inkluderer tredjepartsverktøy som WebStorm eller å bruke den gode gamle `console.log` for å skrive ut tilstander av variabler. Men disse tilbyr ikke den sanntidsinteraksjonen og detaljerte inspeksjonen som feilsøkere gjør.

Når det gjelder implementeringsdetaljer, fungerer de fleste feilsøkere likt: de lar deg sette brytepunkter som pauser utførelsen, gå gjennom kode, inspisere gjeldende variabeltilstander, se på uttrykk, og til og med manipulere verdier på farten for å teste ulike scenarioer.

## Se Også
- [Google Chrome DevTools](https://developers.google.com/web/tools/chrome-devtools)
- [Mozilla Developer Network - Firefox Debugger](https://developer.mozilla.org/en-US/docs/Tools/Debugger)
- [Visual Studio Code - Feilsøking](https://code.visualstudio.com/docs/editor/debugging)
