---
date: 2024-01-20 17:37:45.872161-07:00
description: "How to: Bruk `tr`, `awk`, eller shell parameter expansion til \xE5 gj\xF8\
  re om store bokstaver til sm\xE5. Her er noen eksempler."
lastmod: '2024-03-13T22:44:40.959553-06:00'
model: gpt-4-1106-preview
summary: "Bruk `tr`, `awk`, eller shell parameter expansion til \xE5 gj\xF8re om store\
  \ bokstaver til sm\xE5."
title: "Konvertere en streng til sm\xE5 bokstaver"
weight: 4
---

## How to:
Bruk `tr`, `awk`, eller shell parameter expansion til å gjøre om store bokstaver til små. Her er noen eksempler:

```Bash
# Bruk tr
echo "Hei Verden" | tr '[:upper:]' '[:lower:]'

# Bruk awk
echo "Hei Verden" | awk '{print tolower($0)}'

# Bruk shell parameter expansion i Bash 4.0 eller nyere
text="Hei Verden"
echo "${text,,}"
```

Eksempelutskrift for hvert kommando vil være:
```
hei verden
```

## Deep Dive
Før `tr` og `awk`, var alternativene grunnleggende. Du kunne skrevet ditt eget skript for å itere over hver bokstav, noe som ville være tregt. I Bash, før versjon 4, fantes ikke innebygde alternativer for å endre store bokstaver til små.

Alternativer inkluderer moderne verktøy som `sed` eller programmeringsspråk som Python og Perl, som har denne funksjonaliteten innebygd. Med Bash 4.0+, gjorde `${text,,}` og dens relaterte patterner det mulig å unngå eksterne verktøy for slike string-operasjoner.

Litt om implementering av disse løsningene:

- `tr` er et kraftig og raskt program for tegnoversetting.
- `awk` er et tekstbehandlingsverktøy som også kan utføre komplekse operasjoner på tekst.
- Shell parameter expansion er integrert i Bash, som betyr ingen ekstern prosessering – raskere og mer effektivt.

## See Also
- Bash manualen: https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Shell-Parameter-Expansion
- GNU `tr`: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- AWK-brukerguide: https://www.gnu.org/software/gawk/manual/gawk.html
