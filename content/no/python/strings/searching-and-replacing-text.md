---
date: 2024-01-20 17:58:24.262059-07:00
description: "Slik gj\xF8r du: Fra tidlige Unix-verkt\xF8y som `sed` til moderne programmeringsspr\xE5\
  k, har s\xF8k og erstatt v\xE6rt essensielt. Python's `str.replace()` er enkel\u2026"
lastmod: '2024-04-05T21:53:41.325454-06:00'
model: gpt-4-1106-preview
summary: "Fra tidlige Unix-verkt\xF8y som `sed` til moderne programmeringsspr\xE5\
  k, har s\xF8k og erstatt v\xE6rt essensielt."
title: "S\xF8king og erstatting av tekst"
weight: 10
---

## Slik gjør du:
```python
# Enkle søk og erstatt med str.replace()
tekst = "Hello, verden!"
ny_tekst = tekst.replace("verden", "Norge")
print(ny_tekst)  # Output: Hello, Norge!

# Regulære uttrykk for komplekse mønstre med re.sub()
import re
tekst = "Hjemme best, men 1234 Norge er bedre."
ny_tekst = re.sub(r'\d+', '[nummer]', tekst)
print(ny_tekst)  # Output: Hjemme best, men [nummer] Norge er bedre.
```

## Dykk dypere:
Fra tidlige Unix-verktøy som `sed` til moderne programmeringsspråk, har søk og erstatt vært essensielt. Python's `str.replace()` er enkel men begrenset til nøyaktige tegn. `re` modulen lar derimot programmerere søke etter mønstre gjennom regulære uttrykk.

Alternative løsninger som `awk` og teksteditorer (f.eks. Vim, Emacs) er også kraftige for tekstmanipulering. Python's `str.replace()` er perfekt for raske, enkle erstatninger, mens `re.sub()` lar deg utføre mer komplekse operasjoner der betingelser og mønster-logikk er nødvendig.

Ytelse og effektivitet varierer. Enkle bytter med `str.replace()` er kjappe, mens regulære uttrykk kan være tregere. I store tekstmengder er det viktig å optimalisere uttrykk for å unngå ytelsesproblemer.

## Se også:
- Python's offisielle dokumentasjon om `str.replace()`: https://docs.python.org/3/library/stdtypes.html#str.replace
- Detaljer om regulære uttrykk i Python: https://docs.python.org/3/library/re.html
- En rask introduksjon til `sed`: https://www.gnu.org/software/sed/manual/sed.html
- Vim's hjemmeside for tekstredigering: https://www.vim.org/
- AWK programmeringsspråk guide: https://www.gnu.org/software/gawk/manual/gawk.html
