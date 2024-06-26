---
date: 2024-01-20 17:54:56.129483-07:00
description: "Slik gj\xF8r du: For \xE5 lese en enkel tekstfil, bruk `open()` funksjonen\
  \ sammen med `read()` metoden."
lastmod: '2024-03-13T22:44:40.378386-06:00'
model: gpt-4-1106-preview
summary: "For \xE5 lese en enkel tekstfil, bruk `open()` funksjonen sammen med `read()`\
  \ metoden."
title: Lese en tekstfil
weight: 22
---

## Slik gjør du:
For å lese en enkel tekstfil, bruk `open()` funksjonen sammen med `read()` metoden.

```python
# Åpner og leser innholdet i en tekstfil
with open('eksempel.txt', 'r') as fil:
    innhold = fil.read()
    print(innhold)

# Eksempel på output:
# Dette er innholdet i eksempel.txt filen.
```

For å lese en fil linje for linje:

```python
# Åpner en tekstfil og leser den linje for linje
with open('eksempel.txt', 'r') as fil:
    for linje in fil:
        print(linje.strip())

# Eksempel på output:
# Dette er den første linjen i eksempel.txt.
# Dette er den andre linjen.
```

## Dypdykk
Å lese filer har vært en grunnleggende del av programmering siden de første dagene. I gamle dager, var tilgang til filsystemet saktere og mer begrenset, men metodene var lignende. Nå har vi flere måter å lese filer på, som `read()`, `readline()` og `readlines()` i Python, fra lavnivå file I/O til høynivå biblioteker som `pandas` for CSV filer.

Python lukker automatisk filen etter `with`-blokken er kjørt, noe som unngår mange vanlige feil ved filhåndtering. Det er også mulig å lese filer asynkront for å forbedre ytelse i I/O-begrensede applikasjoner.

## Se Også
- [Python dokumentasjon for filhåndtering](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [w3schools Python File Handling](https://www.w3schools.com/python/python_file_handling.asp)
- [Real Python artikkel om filhåndtering](https://realpython.com/read-write-files-python/)
