---
title:                "Skriving av en tekstfil"
date:                  2024-01-19
html_title:           "Arduino: Skriving av en tekstfil"
simple_title:         "Skriving av en tekstfil"

category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Skriving av tekstfiler gjør at programmer lagrer data. Dette er nyttig til logging, brukerdata eller konfigurasjonsfiler.

## How to:
### Skriv til en fil:
```python
with open('eksempel.txt', 'w') as fil:
    fil.write('Hei Norge!\n')
```

### Legg til tekst i en eksisterende fil:
```python
with open('eksempel.txt', 'a') as fil:
    fil.write('Legger til en linje.\n')
```

### Les den skrevne filen:
```python
with open('eksempel.txt', 'r') as fil:
    innhold = fil.read()
    print(innhold)
```
```shell
Hei Norge!
Legger til en linje.
```

## Deep Dive
Før var `open` og `close` behandlet separat, noe som kunne lede til ressurslekkasjer. Nå bruker vi `with` for automatisk lukking. Alternativer som `pickle` for objektskriving eller databaser som SQLite for strukturerte data finnes. Encoding (standard utf-8) og buffering er viktige implementasjonsdetaljer.

## See Also
- Python dokumentasjon: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
- W3Schools filhåndtering: https://www.w3schools.com/python/python_file_handling.asp
- Real Python tutorial om filskriving: https://realpython.com/read-write-files-python/
