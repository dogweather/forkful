---
title:                "Python: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

#Hvorfor

Sjekking om en mappe eksisterer i et Python-program kan være nyttig når du jobber med filbehandling. Det kan hjelpe deg med å unngå feil og sikre at programmet kjører jevnt.

#Slik gjør du det

```python
# Importer os-modulen for å få tilgang til operativsystemfunksjoner
import os

# Definer en variabel med stien til mappen du vil sjekke
mappe = "/brukere/brukernavn/dokumenter"

# Bruk os.path.exists() for å sjekke om mappen eksisterer
if os.path.exists(mappe):
    print("Mappen eksisterer.")
else:
    print("Mappen eksisterer ikke.")
```

```output
Mappen eksisterer.
```

#Dykk dypere

Hvis du vil vite mer om hvorfor og hvordan man sjekker om en mappe eksisterer i Python, kan du se på os.path modulen. Den inneholder også funksjoner for å sjekke om en fil eksisterer og om en sti er en fil eller mappe.

#Se også

- [Offisiell dokumentasjon for os.path modulen](https://docs.python.org/3/library/os.path.html)
- [Hvordan sjekke om en fil eksisterer i Python](https://www.ionos.com/digitalguide/server/konfigurasjon/sjekk-fil-eksisterer-i-python/)
- [Brukbarheten av å bruke os.path.exists() sammen med shutil modulen i Python](https://itnext.io/reliable-file-and-folder-checking-in-python-eb73309a90f9)