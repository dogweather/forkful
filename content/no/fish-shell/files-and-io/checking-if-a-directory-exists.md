---
title:                "Sjekker om en mappe eksisterer"
date:                  2024-02-03T19:07:21.172837-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sjekker om en mappe eksisterer"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sjekke om en katalog eksisterer i Fish Shell gjør det mulig for skript å ta beslutninger basert på tilstedeværelsen eller fraværet av katalogstrukturer, noe som muliggjør oppgaver som betingede filoperasjoner, logging eller oppsett av miljø. Denne teknikken er avgjørende for å skrive robuste skript som samhandler med filsystemet på en forutsigbar måte.

## Hvordan gjøre det:
Fish Shell bruker `test`-kommandoen til å sjekke filtyper og karakteristikker, inkludert om et mål er en katalog. Her er et grunnleggende mønster for å sjekke om en katalog eksisterer:

```fish
if test -d /sti/til/katalog
    echo "Katalogen eksisterer"
else
    echo "Katalogen eksisterer ikke"
end
```
Eksempel på utskrift:
```
Katalogen eksisterer
```

For mer strømlinjeformede fil- og katalogoperasjoner, kan man vende seg til eksterne verktøy som `fd`, selv om det oftere brukes til å finne filer og kataloger fremfor bare å sjekke for eksistens. Men ved å kombinere det med Fish-scripting, kan man oppnå praktiske resultater:

```fish
set dir "/sti/til/søk"
if fd . $dir --type directory --max-depth 1 | grep -q $dir
    echo "Katalogen eksisterer"
else
    echo "Katalogen eksisterer ikke"
end
```

Dette `fd`-eksempelet søker etter katalogen på en spesifisert dybde, og `grep` sjekker for treff, noe som gjør det nyansert for detaljerte sjekker. Men, for det direkte formålet med å sjekke eksistens, er det å holde seg til Fish sin innebygde `test` både effektivt og rett på sak.
