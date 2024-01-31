---
title:                "Konvertere en streng til små bokstaver"
date:                  2024-01-20T17:38:17.134228-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konvertere en streng til små bokstaver"

category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å konvertere en streng til små bokstaver betyr å endre alle tegn i strengen til deres småbokstav-ekvivalenter. Programmerere gjør dette for å standardisere tekstinput for enkel sammenligning og datahåndtering.

## Hvordan:
```Fish Shell
# Konverter en enkel streng til små bokstaver.
echo "Hei Verden!" | string lower

# Sample output:
hei verden!

# Konverter output fra en kommando
set versjon (fish --version | string lower)
echo $versjon

# Sample output:
fish, versjon 3.x.x
```

## Dypdykk
I gamle dager, før Unicode og komplekse tegnsett, var strengkonvertering rettfram. Nå må moderne verktøy håndtere et bredt spekter av tegn og kodninger. Alternativer til `string lower` inkluderer bruk av `awk`, `tr`, eller programmeringsspråk som Python. I Fish Shell tar `string lower` seg av tungløftingen, og gir en ren syntaks uten behov for eksterne verktøy eller rørledningsakrobatikk.

## Se Også
- Fish dokumentasjon om strengmanipulasjon: https://fishshell.com/docs/current/cmds/string.html
- Unicode case mapping: https://www.unicode.org/reports/tr21/tr21-5.html
- Shell script tutorials: https://www.shellscript.sh
