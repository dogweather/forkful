---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Arduino: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

---

## Hva & Hvorfor?

Å konvertere en streng til småbokstaver innebærer å endre alle tekstens store bokstaver til småbokstaver. Programmerere gjør dette for å standardisere datainput, unngå feil med mellomstore data, og forenkle tekstbehandling.

---

## Hvordan:

I TypeScript kan vi bruke metoden `toLowerCase()` for å konvertere en streng til småbokstaver.

Her er en kodeeksempel:

```
let navn: string = "Henrik";

console.log(navn.toLowerCase());
```

Og dette er output:

```
henrik
```

---

## Dyp Dykk:

Historisk sett har behovet for å konvertere en streng til småbokstaver eksistert for å normalisere data og sørge for at en datamaskin kan forstå og utføre operasjoner riktig uavhengig av inndataskrivemåten.

Alternativt finnes det andre funksjoner som `toLocaleLowerCase()`. Denne metoden konverterer også en tekst til småbokstaver, men tar hensyn til lokale skriveregler. For eksempel, i den tyrkiske alfabet er det små bokstaver som ikke tilsvarer de engelske.

For å konvertere en streng til småbokstaver i TypeScript, lik mange andre programmeringsspråk, utfører `toLowerCase()` -metoden egentlig en iterasjon gjennom hver bokstav i strengen og erstatter den med sin lavere tilsvarende hvis den finnes.

---

## Se også:

2. [JavaScript String toLowerCase() Method](https://www.w3schools.com/jsref/jsref_tolowercase.asp)