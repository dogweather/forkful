---
date: 2024-01-19
description: "Skriving til standard error (stderr) er hvordan programmer rapporterer\
  \ feil og advarsler uten \xE5 blande dette med hoveddatautstr\xF8mmen (stdout).\u2026"
lastmod: 2024-02-19 22:04:59.659680
summary: "Skriving til standard error (stderr) er hvordan programmer rapporterer feil\
  \ og advarsler uten \xE5 blande dette med hoveddatautstr\xF8mmen (stdout).\u2026"
title: Skrive til standardfeil
---

{{< edit_this_page >}}

## What & Why?
Skriving til standard error (stderr) er hvordan programmer rapporterer feil og advarsler uten å blande dette med hoveddatautstrømmen (stdout). Programmører gjør dette for å skille normal output fra feilmeldinger, noe som gjør det enklere å logge og diagnostisere problemer.

## How to:
For å skrive til stderr i Python, bruk `sys.stderr`. Her er et enkelt eksempel:

```Python
import sys

print("Dette er en normal melding")
sys.stderr.write("Dette er en feilmelding\n")
```

Forventet output:
```
Dette er en normal melding
Dette er en feilmelding
```

Merk at `stderr`-meldingen kan dukke opp et annet sted enn den vanlige output, avhengig av hvordan skriptet ditt kjøres.

## Deep Dive
Historisk sett er konseptet med standard error en del av Unix filstrømningsmodellen hvor alt er en fil, inkludert outputstrømmer. Alternativer til `sys.stderr` inkluderer logging-biblioteker som lar deg styrke fleksibiliteten og konfigurasjonen av feilloggningen din. Implementationen av stderr i Python er bare en høy-nivå tilnærming til fil-deskriptoren som er tilgjengelig i lav-nivå operativsystemets grensesnitt.

## See Also
- Python's official `sys` module documentation: https://docs.python.org/3/library/sys.html
- Python's logging library guide: https://docs.python.org/3/library/logging.html
- Unix philosophy on streams: https://en.wikipedia.org/wiki/Unix_philosophy#Everything_is_a_file
