---
date: 2024-01-20 17:45:50.299406-07:00
description: "Hvordan: Her er et par Fish Shell-eksempler: 1. Bruk `string`-kommandoen\
  \ til \xE5 klippe ut en delstreng fra en posisjon."
lastmod: '2024-03-13T22:44:41.214161-06:00'
model: gpt-4-1106-preview
summary: Her er et par Fish Shell-eksempler.
title: Uthenting av delstrenger
weight: 6
---

## Hvordan:
Her er et par Fish Shell-eksempler:

1. Bruk `string`-kommandoen til å klippe ut en delstreng fra en posisjon:

```Fish Shell
set streng "Fiskesuppe er deilig"
set delstreng (string sub -s 1 -l 11 $streng)
echo $delstreng  # Output: Fiskesuppe
```

2. Hent en del av en streng via indekser:

```Fish Shell
set streng "Fiskesuppe er deilig"
set delstreng (string sub -s 14 -l 7 $streng)
echo $delstreng  # Output: deilig
```

3. Ekskluder spesifikke tegn:

```Fish Shell
set streng "Fiskesuppe er deilig"
set delstreng (string sub -s 1 --length (math (string length $streng) - 7) $streng)
echo $delstreng  # Output: Fiskesuppe er
```

## Dyptdykk
I tidligere skall som Bash brukte vi variabelmanipulering og kommandoer som `cut` for å trekke ut delstrenger. Fish Shell moderniserte dette med den innebygde `string`-kommandoen, som tilbyr intuitive argumenter for å utføre strengoperasjoner.

Alternativer inkluderer å bruke `awk`, `sed`, eller ren Perl og Python for mer kompleks behandling av tekst. Hvert verktøy har sine egne styrker, men Fish sin `string`-kommando gir et lettforståelig og raskt alternativ.

Når vi trekker ut delstrenger, opererer vi med en 1-indeksert posisjon, som er mer likt naturlig tellemetode og dermed kan føles mer intuitivt enn 0-indekserte språk.

## Se Også
- Fish Shell dokumentasjon på `string`: https://fishshell.com/docs/current/cmds/string.html
- Tutorial for strengmanipulering i Fish: https://www.fishshell.com/docs/current/tutorial.html#tut_strings
- Sammenligning av tekstbehandlingskommandoer i ulike shell: https://en.wikibooks.org/wiki/UNIX_Shell_Scripting#String_processing
