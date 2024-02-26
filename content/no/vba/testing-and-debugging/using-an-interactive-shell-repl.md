---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:51.220635-07:00
description: "Et interaktivt skall, eller Read-Eval-Print Loop (REPL), lar brukere\
  \ skrive inn kommandoer, utf\xF8re dem og se resultatene i sanntid. Programmerere\
  \ benytter\u2026"
lastmod: '2024-02-25T18:49:38.803145-07:00'
model: gpt-4-0125-preview
summary: "Et interaktivt skall, eller Read-Eval-Print Loop (REPL), lar brukere skrive\
  \ inn kommandoer, utf\xF8re dem og se resultatene i sanntid. Programmerere benytter\u2026"
title: "\xC5 bruke et interaktivt skall (REPL)"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Et interaktivt skall, eller Read-Eval-Print Loop (REPL), lar brukere skrive inn kommandoer, utføre dem og se resultatene i sanntid. Programmerere benytter REPL for rask prototyping, testing av kodeutdrag, eller feilsøking i et mer interaktivt og iterativt miljø, forbedrer produktiviteten og forståelsen av koden.

## Hvordan:

Visual Basic for Applications (VBA) støtter ikke naturlig et interaktivt skall eller REPL-opplevelse som sett i språk som Python eller JavaScript. Du kan imidlertid i en viss grad simulere denne opplevelsen ved å bruke umiddelbart vindu i VBA IDE (Integrated Development Environment).

**Tilgang til umiddelbart vindu:**
1. Åpne VBA IDE ved å trykke `Alt + F11` i Office-applikasjonen din.
2. Hvis umiddelbart vindu ikke er synlig, kan du åpne det ved å trykke `Ctrl + G` eller velge det fra Vis-menyen.

**Bruke umiddelbart vindu som en REPL:**
- For å utføre en linje med kode, skriv den ganske enkelt inn i umiddelbart vindu og trykk Enter. For eksempel:

```basic
Debug.Print 2 + 2
```

- Eksempel på utdata:
```
 4
```

- Du kan også kalle funksjoner og subrutiner definert i modulene dine:

```basic
Public Sub SayHello()
    Debug.Print "Hello, World!"
End Sub
```

- Og deretter i umiddelbart vindu:
```basic
Call SayHello
```

- Eksempel på utdata:
```
 Hello, World!
```

**Merk:** Umiddelbart vindu har begrensninger. Det er utmerket for raske tester og direkte funksjonskall, men det støtter ikke å definere funksjoner eller subrutiner direkte inni det. Kompleks feilsøking og programmeringsoppgaver kan kreve full modulutvikling.

## Dypdykk

Umiddelbart vindu i VBA tjener som det nærmeste motstykket til interaktive skall funnet i andre programmeringsøkosystemer, til tross for dets begrensninger. Historisk sett har VBA fokusert på å utvide kapasitetene til Microsoft Office-applikasjoner gjennom skript og makroer heller enn selvstendig programvareutvikling, noe som kan forklare fraværet av et fullverdig REPL.

For oppgaver som krever omfattende interaktiv testing eller kompleks logikkutvikling, kan andre programmeringsmiljøer utstyrt med støtte for native REPL, som Python med sin IDLE, eller JavaScript med Node.js, tilby bedre alternativer. Disse miljøene gir ikke bare interaktive skall, men også mer robust programmering, feilsøking og testing fasiliteter.

Umiddelbart vindu gir likevel et uvurderlig verktøy for å kjapt teste uttrykk, kjøre funksjoner og direkte manipulere objekter i Office-applikasjoner. Som sådan, opptar det en vital nisje innen VBA-utviklingsprosessen, og tilbyr en umiddelbarhet og bekvemmelighet som er uovertruffen av mer tradisjonelle kompiler-kjør-feilsøk sykluser, selv med de forståtte begrensningene av dens operasjonelle omfang.
