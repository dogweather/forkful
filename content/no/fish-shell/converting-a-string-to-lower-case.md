---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Fish Shell: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

Hva & hvorfor?
Konvertering av en streng til små bokstaver er en vanlig praksis blant programmerere for å gjøre tekstbehandling og sammenligning av tekst mer pålitelig. Dette gjøres ved å endre alle bokstavene til små bokstaver, noe som gjør det enklere å sammenligne to tekster uten å bekymre seg for forskjeller i store og små bokstaver.

Slik gjør du:
Fish Shell tilbyr en enkel kommando for å konvertere en streng til små bokstaver: `lowercase`. Her er et eksempel på hvordan du kan bruke denne kommandoen:

```
Fish Shell ~> lowercase "HeLlO wOrLD"
hello world
```

Resultatet av å kjøre denne kommandoen vil være en streng med alle bokstaver konvertert til små bokstaver. Dette kan også gjøres med variabler eller output fra andre kommandoer, for eksempel:

```
Fish Shell ~> set greeting "Hei, Velkommen!"
Fish Shell ~> lowercase $greeting
hei, velkommen!
```

Dypdykk:
Å konvertere en streng til små bokstaver er en vanlig praksis som har eksistert i lang tid blant programmerere. Dette sikrer at sammenligninger av tekst er nøyaktige og pålitelige uavhengig av hvilken plattform eller programmeringsspråk som brukes.

Som en alternativ metode kan noen programmeringsspråk også tilby en innebygd funksjon for å konvertere en streng til små bokstaver, som for eksempel `toLowerCase()` i JavaScript.

I Fish Shell blir konvertering av en streng til små bokstaver gjort ved hjelp av POSIX-funksjonen `tolower`, som gjør bruken av denne kommandoen rask og effektiv.

Se også:
Hvis du ønsker å lære mer om andre nyttige kommandoer og funksjoner i Fish Shell, kan du sjekke ut dokumentasjonen på deres offisielle nettside: https://fishshell.com/docs/current/

Du kan også utforske mulighetene og tilpasningsalternativene i Fish Shell ved å se på eksempler og diskusjoner på deres GitHub-samfunn: https://github.com/fish-shell/fish-shell