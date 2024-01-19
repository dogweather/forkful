---
title:                "Opprette en midlertidig fil"
html_title:           "C#: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Opprettelse av midlertidige filer i Python

## Hva & Hvorfor?
Å lage en midlertidig fil betyr å lage en fil som bare lever i en kort tid. Programmerere gjør det for å lagre data midlertidig, for eksempel for å dele data mellom forskjellige prosesser i koden.

## Hvordan:
Hvordan du oppretter en midlertidig fil i Python? Her er hvordan:
```Python
import tempfile

# Opprette en midlertidig fil
tmp = tempfile.TemporaryFile() 

# Skrive til den midlertidige filen
tmp.write(b'Some data')

# Tilbake til start for å lese data
tmp.seek(0)

# Lees data
print(tmp.read())
```
Sample output:
```Python
b'Some data'
```
## Dyp Dykk:
Historisk sett, har teknikken å lage midlertidige filer vært nyttig for mange prosedyrer, som sortering og hashing. Men, det er noen alternativer til midlertidige filer, som bruk av minnet. Implementeringen av midlertidige filer i Python (og mange andre språk) er gjort via operativsystemet.

## Se også:
For mer informasjon om midlertidige filer i Python, se Pythons offisielle dokumentasjon: https://docs.python.org/3/library/tempfile.html