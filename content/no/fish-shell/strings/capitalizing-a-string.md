---
aliases:
- /no/fish-shell/capitalizing-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:19.170453-07:00
description: "\xC5 sette stor forbokstav i en streng betyr \xE5 endre den slik at\
  \ den f\xF8rste bokstaven er stor, og resten av strengen er med sm\xE5 bokstaver.\
  \ Dette er en vanlig\u2026"
lastmod: 2024-02-18 23:08:54.333671
model: gpt-4-0125-preview
summary: "\xC5 sette stor forbokstav i en streng betyr \xE5 endre den slik at den\
  \ f\xF8rste bokstaven er stor, og resten av strengen er med sm\xE5 bokstaver. Dette\
  \ er en vanlig\u2026"
title: Sette stor bokstav i en streng
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sette stor forbokstav i en streng betyr å endre den slik at den første bokstaven er stor, og resten av strengen er med små bokstaver. Dette er en vanlig oppgave i tekstbehandling, normalisering av brukerinndata, og dataformatering for å sikre konsistens eller for å møte spesifikke formateringskriterier.

## Hvordan:

I Fish Shell kan strenger manipuleres direkte med innebygde funksjoner, uten behov for eksterne verktøy eller biblioteker. For å sette stor forbokstav i en streng, kan du kombinere `string`-kommandoen med underkommandoer.

```fish
# Eksempelstreng
set sample_string "hello world"

# Stor forbokstav på første bokstav
set capitalized_string (string sub -l 1 -- $sample_string | string upper)(string sub -s 2 -- $sample_string)

echo $capitalized_string
```

Output:
```
Hello world
```

For scenarier som krever at flere ord i en streng begynner med stor bokstav (for eksempel, konvertere "hello world" til "Hello World"), ville du iterere over hvert ord, og anvende logikken for å sette stor bokstav på hvert:

```fish
# Eksempelsetning
set sentence "hello fish shell programming"

# Stor bokstav på hvert ord
set capitalized_words (string split " " -- $sentence | while read -l word; string sub -l 1 -- $word | string upper; and string sub -s 2 -- $word; end)

# Koble sammen de store bokstavene
set capitalized_sentence (string join " " -- $capitalized_words)

echo $capitalized_sentence
```

Output:
```
Hello Fish Shell Programming
```

Merk at Fish Shell ikke direkte tilbyr en enkel-kommando tilnærming for setting av stor forbokstav i hele setninger på samme måte som noen programmeringsspråk gjør med deres strengmetoder. Derfor representerer kombineringen av `string split`, `string sub`, `string upper`, og deretter sammenføyning en idiomatisk tilnærming i Fish Shell for å oppnå dette.
