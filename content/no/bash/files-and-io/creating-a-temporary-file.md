---
date: 2024-01-20 17:39:24.573368-07:00
description: "Hvordan gj\xF8r man det: Opprettelsen av midlertidige filer i UNIX-lignende\
  \ systemer har lenge v\xE6rt vanlig, gjort med verkt\xF8y som `mktemp`, som ble\u2026"
lastmod: '2024-04-05T22:50:54.996586-06:00'
model: gpt-4-1106-preview
summary: "Opprettelsen av midlertidige filer i UNIX-lignende systemer har lenge v\xE6\
  rt vanlig, gjort med verkt\xF8y som `mktemp`, som ble tilgjengelig p\xE5 1980-tallet."
title: Opprette en midlertidig fil
weight: 21
---

## Hvordan gjør man det:
```Bash
# Opprett en midlertidig fil med mktemp-kommandoen
temp_file=$(mktemp)
echo "Dette er en test" > "${temp_file}"

# Vis innholdet i den midlertidige filen
cat "${temp_file}"

# Slett den midlertidige filen etter bruk
rm "${temp_file}"
```
Eksempelutskrift:
```
Dette er en test
```

## Dypdykk
Opprettelsen av midlertidige filer i UNIX-lignende systemer har lenge vært vanlig, gjort med verktøy som `mktemp`, som ble tilgjengelig på 1980-tallet. Tidligere var det vanlig å bruke `$$` (prosessens ID) for å navngi midlertidige filer, men dette kunne lede til sikkerhetsproblemer. `mktemp` løser dette ved å skape en unik fil i `/tmp`-katalogen med en tilfeldig del i navnet. En alternativ måte er å bruke `tempfile`-kommandoen, skjønt `mktemp` er oftere anbefalt grunnet større fleksibilitet og bredere tilgjengelighet. Implementeringsdetaljene til `mktemp` kan inkludere et prefiks, suffiks, og spesifisering av katalog.

## Se også
- Bash-håndboken: https://www.gnu.org/software/bash/manual/
- `mktemp` man-side: https://linux.die.net/man/1/mktemp
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/
