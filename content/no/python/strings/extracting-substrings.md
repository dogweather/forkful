---
date: 2024-01-20 17:46:12.556831-07:00
description: "How to: Python gj\xF8r det enkelt \xE5 hente ut delstrenger. Se p\xE5\
  \ slice-syntaksen."
lastmod: '2024-03-13T22:44:40.350070-06:00'
model: gpt-4-1106-preview
summary: "Python gj\xF8r det enkelt \xE5 hente ut delstrenger."
title: Uthenting av delstrenger
weight: 6
---

## How to:
Python gjør det enkelt å hente ut delstrenger. Se på slice-syntaksen:

```python
tekst = "NorskPythonProgrammering"
delstreng = tekst[6:12]
print(delstreng) # Output: Python
```

Eller trekk ut med `split()` og array-indexer:

```python
tekst = "Hello, world! Velkommen!"
ord_liste = tekst.split()
delstreng = ord_liste[2]
print(delstreng) # Output: Velkommen!
```

## Deep Dive
I Python startet vi med enkle substrings via slicing helt tilbake i de første versjonene. Alternativer inkluderer bruken av `slice()`-objekt, `substr()` i andre språk, og regex for komplekse mønstre. Slicing i Python er raskt og minneeffektivt siden det returnerer en "view" og ikke en kopi av delstrengen.

## See Also
- Python sin offisielle dokumentasjon: https://docs.python.org/3/library/stdtypes.html#string-methods
- En dypere dykk i slicing: https://realpython.com/lessons/indexing-and-slicing/
- Regex-guide for avanserte strengoperasjoner: https://docs.python.org/3/howto/regex.html
