---
title:                "Sjekke om en mappe eksisterer"
html_title:           "Python: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva Og Hvorfor?
Å sjekke om en mappe eksisterer er en viktig del av programmering. Dette betyr å sjekke om en spesifikk mappe er tilgjengelig og kan brukes i koden din. Programmører gjør dette for å sikre at koden kjører jevnt og forhindrer feil og uønskede resultater.

## Hvordan:
For å sjekke om en mappe eksisterer, bruker vi ```os.path.exists()``` funksjonen i Pythons operativsystemmodul. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```
import os

if os.path.exists("min_mappe"):
    print("Mappen eksisterer")
else:
    print("Mappen eksisterer ikke")
```
Eksempelutskrift:
```
Mappen eksisterer
```

## Dypdykk:
Sjekking av eksistensen til en mappe kommer fra UNIX-systemer, hvor filsystemer er hierarkiske og alle filer og mapper er plassert i en rot-mappe. Det finnes også andre måter å sjekke om en mappe eksisterer på, som å bruke systemkommandoer. Men å bruke ```os.path.exists()``` funksjonen er den anbefalte måten i Python.

## Se Også:
Du kan lære mer om Python operativsystemmodul i [dokumentasjonen.](https://docs.python.org/3/library/os.html) For å lære mer om UNIX-filsystemer og kommandoer, kan du sjekke ut [dette nettstedet.](https://www.tldp.org/LDP/intro-linux/html/sect_03_02.html)