---
title:                "Skrive en tekstfil"
html_title:           "Elm: Skrive en tekstfil"
simple_title:         "Skrive en tekstfil"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?
Å skrive en tekstfil er en måte for programmerere å lagre informasjon på en strukturert måte. Det lar deg lese og skrive data til en fil som kan hentes senere i koden din.

Hvordan:
Elm har en innebygd funksjon kalt "text" som gjør at du kan skrive tekst til en fil. Denne funksjonen tar inn en tekststreng som parameter og lagrer det i en fil med navnet du angir. Her er et eksempel på hvordan du bruker denne funksjonen:
```
Elm.text "Hello World!" "hello.txt"
```
Dette vil skrive teksten "Hello World!" til en fil med navnet "hello.txt" i samme mappe som din Elm-fil.

Dypdykk:
Tekstfilen har eksistert siden begynnelsen av datamaskinens tidsalder og brukes fortsatt mye i dag. Alternativene til å skrive til en tekstfil i Elm inkluderer å bruke porter, som lar deg kommunisere med eksterne programmer, eller å bruke et annet programmeringsspråk som støtter tekstfilmanipulasjon. Implementeringen av å skrive til en tekstfil i Elm er enkel på grunn av "text" funksjonen som håndterer det meste av arbeidet for deg.

Se også:
Foreløpig er det ingen offisielle dokumentasjon for "text" funksjonen, men det er mange ressurser på nettet som kan hjelpe deg med å lære mer om hvordan du kan bruke den og håndtere tekstfiler generelt. Her er noen lenker som kan være nyttige:
- Offisiell Elm-dokumentasjon: https://elm-lang.org/docs
- Stack Overflow: https://stackoverflow.com/questions/tagged/elm
- Elm Slack-kanalen: https://elmlang.herokuapp.com/