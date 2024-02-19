---
aliases:
- /no/python/concatenating-strings/
date: 2024-01-20 17:35:40.163379-07:00
description: "I Python er string-sammenf\xF8yning prosessen \xE5 sette sammen to eller\
  \ flere tekststrenger. Det gj\xF8res for \xE5 bygge setninger, kombinere brukerinput,\
  \ eller\u2026"
lastmod: 2024-02-18 23:08:53.516703
model: gpt-4-1106-preview
summary: "I Python er string-sammenf\xF8yning prosessen \xE5 sette sammen to eller\
  \ flere tekststrenger. Det gj\xF8res for \xE5 bygge setninger, kombinere brukerinput,\
  \ eller\u2026"
title: "Sammensl\xE5ing av strenger"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
I Python er string-sammenføyning prosessen å sette sammen to eller flere tekststrenger. Det gjøres for å bygge setninger, kombinere brukerinput, eller generere dynamisk innhold.

## Hvordan:
I eksemplene nedenfor, bruker vi ulike måter å slå sammen strenger. Merk at output kommer rett etter koden.

```Python
# Bruk av + operatør
hilsen = "Hei, " + "verden!"
print(hilsen)
```
Output: Hei, verden!

```Python
# Joining strings with join()
navneliste = ["Ola", "Kari", "Nils"]
print(", ".join(navneliste))
```
Output: Ola, Kari, Nils

```Python
# Formatering med f-strings (tilgjengelig fra Python 3.6 og oppover)
navn = "Anne"
melding = f"Hei {navn}, hvordan går det?"
print(melding)
```
Output: Hei Anne, hvordan går det?

## Dypdykk
Den direkte sammenføyningen av strenger med `+` er intuitiv, men ikke alltid effektiv, særlig med store eller mange strenger. Historisk sett, før f-strings ble introdusert i Python 3.6, var format() metoden eller %-formatering populære valg, slik som ` "Hei %s" % navn` eller `"Hei {}".format(navn)`. 

Egentlig holder Python-strenger i seg selv som 'immutable', noe som betyr at hver gang du bruker `+`, lager Python en ny streng. Dette kan føre til betydelig overhead med store datamengder. Derfor er ofte `''.join(iterable)` mer effektivt, fordi det bygger strengen i ett enkelt steg.

F-strings, altså formaterte strengliteraler (introdusert i Python 3.6), gir en raskere og mer leselig måte å formatere strenger på. Med f-strings, embedder du Python-uttrykk direkte i streng-literaler ved å prefikse strengen med en `f` og skrive uttrykket inni `{}`.

## Se Også
- Python offisiell dokumentasjon på strings: https://docs.python.org/3/library/string.html
- PEP 498 om formaterte strengliteraler (f-strings): https://www.python.org/dev/peps/pep-0498/
- The Pragmatic Programmer's Guide to Python: https://docs.python-guide.org/writing/style/
