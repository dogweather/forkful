---
title:                "Fish Shell: Store bokstaver i en streng"
simple_title:         "Store bokstaver i en streng"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Stringer (tekststrenger) er en vanlig del av enhver programmeringsoppgave. Ofte ønsker man å skille mellom små og store bokstaver for å gjøre teksten mer lesbar eller for å følge programmeringsstandarder. Med Fish Shell er det enkelt å gjøre dette ved å bruke kommandoen "string capitalize". Men hvorfor skulle man ønske å gjøre dette i utgangspunktet? 

## Hvordan

```Fish Shell
set streng "hallo verden"
string capitalize $streng
```

I dette eksempelet definerer vi en variabel kalt "streng" som inneholder teksten "hallo verden". Deretter bruker vi kommandoen "string capitalize" for å gjøre om "hallo verden" til "Hallo verden". Dette er nyttig når man ønsker å markere en tittel eller en setning i en tekst, eller når man ønsker å følge en konsistent formatering i koden sin.

En annen mulighet er å bruke "string capitalize" på variabler som inneholder navn eller initialer. Dette vil gjøre det enklere å lese og identifisere navnene i koden.

## Deep Dive

Kommandoen "string capitalize" følger Unicode-standarder, som betyr at den også fungerer med bokstaver fra andre språk enn engelsk. Dette gjør det enkelt å formatere tekster på norsk, samisk eller andre språk med spesielle tegn.

I tillegg til "string capitalize" finnes det også andre kommandoer i Fish Shell for å håndtere tekster, som for eksempel "string lower" for å gjøre alle bokstavene i en tekst til små, og "string trim" for å fjerne ekstra mellomrom i en tekst.

## Se også

* [Fish Shell sin offisielle dokumentasjon](https://fishshell.com/docs/current/cmds/string.html)
* [En oversikt over alle Fish Shell sine tekstkommandoer](https://fishshell.com/docs/current/commands.html#string)
* [En guide til å lære Fish Shell](https://www.hostinger.no/skrevet/fish-shell-101-getting-started-guide) (på norsk)