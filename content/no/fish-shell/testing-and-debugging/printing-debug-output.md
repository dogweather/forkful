---
date: 2024-01-20 17:52:42.813239-07:00
description: "Debugging-utskrifter er info vi skriver ut for \xE5 forst\xE5 hva koden\
  \ v\xE5r egentlig gj\xF8r. Programmerere bruker dette for \xE5 finne bugs eller\
  \ forst\xE5 flyten\u2026"
lastmod: '2024-03-13T22:44:41.229511-06:00'
model: gpt-4-1106-preview
summary: "Debugging-utskrifter er info vi skriver ut for \xE5 forst\xE5 hva koden\
  \ v\xE5r egentlig gj\xF8r."
title: "Skrive ut feils\xF8kingsdata"
weight: 33
---

## Hvordan:
I Fish Shell, bruk `echo` eller `printf` for å skrive ut debug-informasjon. Legg til `-d` for detaljert output.

```Fish Shell
# Enkel utskrift
echo "Her er en verdi: $variabel"

# Formater med printf
set antall 42
printf "Det er %d epler i kurven.\n" $antall

# Skriv ut kommandoers resultater
echo "Nåværende katalog er: "(pwd)

# Detaljert utskrift av en variabel (kun i Fish)
echo $variabel | string collect -d 
```

Eksempelutskrift:
```
Her er en verdi: 3
Det er 42 epler i kurven.
Nåværende katalog er: /brukerens/hjemmekatalog
```

## Dypdykk:
Før Fish var Bash kongen av shell-scripting. Der brukte folk `echo` eller `printf` på lignende måter, men Fish tilbyr enkel syntaks og innebygde funksjoner som gjør scripting mer intuitivt. For eksempel, `string collect -d` i Fish hjelper med å feilsøke ved å vise detaljert info om tekststrengen. Mens alternativer i andre shells krever mer verbose teknikker eller eksterne verktøy.

Implementasjonsdetaljer som er verdt å merke seg inkluderer Fish's moderne skriptsyntax, eksempelvis bruk av parenteser `( )` istedenfor backticks `` ` ` ` eller `$()` for kommando-substitusjon. Dette gjør det klarere hva som skjer.

## Se Også:
- Fish's offisielle dokumentasjon om debugging: https://fishshell.com/docs/current/commands.html#printf
- StackOverflow for Fish Shell tips og triks: https://stackoverflow.com/questions/tagged/fish
- GitHub repo for Fish Shell med mye eksempelkode: https://github.com/fish-shell/fish-shell
