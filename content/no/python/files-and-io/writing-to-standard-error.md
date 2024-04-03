---
date: 2024-01-19
description: "How to: For \xE5 skrive til stderr i Python, bruk `sys.stderr`. Her\
  \ er et enkelt eksempel."
lastmod: '2024-03-13T22:44:40.377482-06:00'
model: unknown
summary: "For \xE5 skrive til stderr i Python, bruk `sys.stderr`."
title: Skrive til standardfeil
weight: 25
---

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
