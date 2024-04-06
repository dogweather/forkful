---
date: 2024-01-20 17:38:17.134228-07:00
description: "Hvordan: I gamle dager, f\xF8r Unicode og komplekse tegnsett, var strengkonvertering\
  \ rettfram. N\xE5 m\xE5 moderne verkt\xF8y h\xE5ndtere et bredt spekter av tegn\
  \ og\u2026"
lastmod: '2024-04-05T21:53:42.171585-06:00'
model: gpt-4-1106-preview
summary: "I gamle dager, f\xF8r Unicode og komplekse tegnsett, var strengkonvertering\
  \ rettfram."
title: "Konvertere en streng til sm\xE5 bokstaver"
weight: 4
---

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
