---
title:                "Skrive ut feilsøkingsdata"
date:                  2024-01-20T17:52:42.813239-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skrive ut feilsøkingsdata"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Debugging-utskrifter er info vi skriver ut for å forstå hva koden vår egentlig gjør. Programmerere bruker dette for å finne bugs eller forstå flyten bedre, spesielt når ting ikke fungerer som de skal.

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