---
title:                "Konvertere en streng til små bokstaver"
date:                  2024-01-20T17:37:45.872161-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Endre tekststrenger til små bokstaver, fordi det sikrer konsistent datahåndtering. Enten for å sammenligne tekster eller for estetikkens skyld, kommer du til å trenge det.

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