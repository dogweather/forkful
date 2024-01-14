---
title:    "Python: Sjekke om en mappe eksisterer"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Hvorfor

Å sjekke om en mappe eksisterer er en viktig del av Python programmering. Det kan hjelpe deg med å sikre at koden din fungerer som den skal og unngå feilmeldinger.

## Hvordan

Det er enkelt å sjekke om en mappe eksisterer ved hjelp av noen få linjer med Python kode. Du kan bruke `os` biblioteket og `path.exists()` funksjonen for å sjekke om mappen eksisterer. Her er et eksempel:

```Python
import os

# Sjekker om mappen "bilder" eksisterer
if os.path.exists("bilder"):
    print("Mappen eksisterer!")
else:
    print("Mappen eksisterer ikke.")
```

Output:
```
Mappen eksisterer!
```

Hvis mappen ikke eksisterer, vil utskriften være "Mappen eksisterer ikke." Dette enkle eksempelet viser hvordan du kan bruke `path.exists()` funksjonen for å sjekke om en mappe eksisterer. Du kan også bruke `path.isdir()` funksjonen for å sjekke om det er en mappe og ikke en fil med samme navn.

## Dykk dypere

Når du bruker `path.exists()` funksjonen, vil den returnere `True` hvis en mappe (eller fil) med samme navn eksisterer og `False` hvis den ikke finnes. Det er viktig å merke seg at denne funksjonen bare sjekker om mappen eksisterer i den nåværende arbeidsmappen. Hvis mappen er i en annen mappe, må du inkludere den fulle stien til mappen i funksjonen.

Det er også mulig å bruke `try-except` blokken for å håndtere eventuelle feilmeldinger som kan oppstå når du sjekker for eksisterende mapper eller filer. Dette gir en mer robust løsning og unngår at koden din krasjer hvis noe går galt.

## Se også

Her er noen nyttige ressurser for å lære mer om å sjekke om mapper eksisterer i Python:

- [Offisiell dokumentasjon for os-biblioteket](https://docs.python.org/3/library/os.html)
- [Slik sjekker du om en fil eller mappe eksisterer i Python](https://myhydropi.com/checking-folder-exits-python/)
- [Slik håndterer du unntak i Python](https://realpython.com/python-exceptions/)