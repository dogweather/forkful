---
date: 2024-01-20 17:48:01.415249-07:00
description: "How to: (Slik gj\xF8r du:) Python bruker `len()` funksjonen. Se eksempel."
lastmod: '2024-03-13T22:44:40.352078-06:00'
model: gpt-4-1106-preview
summary: Python bruker `len()` funksjonen.
title: "Finn lengden p\xE5 en streng"
weight: 7
---

## How to: (Slik gjør du:)
Python bruker `len()` funksjonen. Se eksempel:

```python
tekst = "Hallo, Norge!"
lengde = len(tekst)
print(lengde)
```

Resultat:

```
13
```

## Deep Dive (Dypdykk)
Historisk sett har `len()` alltid vært del av Python. Det er direkte og effektivt, mye fordi det gir et tall som representerer antall elementer i en beholder, og strenger regnes som beholdere. Alternativer inkluderer løkker eller list comprehension for å telle manuelt, men disse metoder er overflødig når `len()` finnes.

Implementasjonen av `len()` er interessant. Python's strengobjekter har allerede en kjent lengde, lagret internt, så `len()` oppslag er O(1) – constant time. Ingen gjennomgang av hvert tegn nødvendig.

## See Also (Se også)
- Python's offisielle dokumentasjon på strenger: https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str
- Et dybde innblikk av Python's data modell (inkludert `len()`): https://docs.python.org/3/reference/datamodel.html
- W3Schools Python String Length: https://www.w3schools.com/python/ref_func_len.asp
