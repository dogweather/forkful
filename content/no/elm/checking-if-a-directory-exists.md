---
title:                "Sjekker om en katalog eksisterer"
html_title:           "Elm: Sjekker om en katalog eksisterer"
simple_title:         "Sjekker om en katalog eksisterer"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Undersøkelse for eksistensen av en mappe er en kodingsteknikk hvor vi bekrefter om en bestemt mappe eksisterer i systemet eller ikke. Programmerere gjør dette for å hindre eventuelle feil, slik som å skrive til en mappe som ikke finnes.

## Hvordan gjøre
Elm gir dessverre ingen innebygget funksjonalitet til å sjekke om en mappe eksisterer direkte. Dette skyldes at Elm er renVirtuell og bør ikke ha noen side-effekt slik som å lese filsystemet. Vi bør benytte server API-er eller JavaScript interop for denne oppgaven. Her er et eksempel på hvordan det kan gjøres ved hjelp av Node.js og Elm:

```javascript
var fs = require('fs');
var exists = fs.existsSync('/path/to/directory');
```

Vi kan så overføre denne informasjonen til Elm.

## Dypdykk

Begrepet å sjekkke om en mappe eksisterer ble populært i dagene av strukturert programmering, når programmerere begynte å jobbe med filsystemer. Til tross for mangel på innebygget støtte i Elm, finnes det alternativt teknikker, som for eksempel å bruke JavaScript Interop, eller ved å få bakenden til å gjøre jobben.

Hovedidéen bak å ikke tillate direkte filsystemtilgang i Elm er å holde den ren og sørge for at all kode er predikerbar og testbar. Elm koder kjører i nettlesermiljøet, og av sikkerhetsgrunner er tilgang til filsystemet sterkt begrenset.

## Se Også
1. [Elm guide for interoperabilitet med JavaScript](https://guide.elm-lang.org/interop/)
2. [Node.js filsystem API](https://nodejs.org/api/fs.html) for hvordan du interagerer med filsystemet via JavaScript.