---
title:    "Python: Sjekke om en mappe eksisterer"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor
Å sjekke om en mappe eksisterer kan være en nyttig ferdighet når vi jobber med å organisere og håndtere filer i programmering. Ved å sjekke om en mappe eksisterer, kan vi unngå feil og problemer i våre kodeløsninger.

## Hvordan
For å sjekke om en mappe eksisterer i Python, kan vi bruke funksjonen ```os.path.exists()```. Denne funksjonen tar inn en sti til en mappe som parameter, og returnerer en sann eller falsk verdi basert på om mappen eksisterer eller ikke. La oss se på et eksempel:

```Python
import os

mappe = "/bruker/desktop/prosjektmappe"

if os.path.exists(mappe):
    print("Mappen eksisterer.")
else:
    print("Mappen eksisterer ikke.")
```

I dette eksempelet bruker vi funksjonen ```exists()``` til å sjekke om mappen "prosjektmappe" eksisterer på skrivebordet. Om mappen eksisterer, vil det bli skrevet ut "Mappen eksisterer.", ellers vil det bli skrevet ut "Mappen eksisterer ikke.". Dette er en enkel måte å sikre at programmet vårt ikke krasjer når vi prøver å jobbe med en mappe som ikke finnes.

## Dypdykk
I tillegg til å bruke ```exists()``` funksjonen, kan vi også utforske ```os.path``` modulen for å utføre andre operasjoner med stier og mapper. For eksempel kan vi bruke funksjonen ```os.path.isdir()``` for å sjekke om en gitt sti peker til en mappe eller en fil. Dette kan være nyttig når vi må håndtere forskjellige typer filer i programmene våre.

Det kan også være lurt å inkludere feilhåndtering når vi sjekker om en mappe eksisterer. Dette kan gjøres ved å bruke ```try``` og ```except``` blokker for å fange eventuelle feil som kan oppstå.

## Se også
- [Python: Check if a File or Directory Exists (Real Python)](https://realpython.com/exists-function-python/)
- [Python os.path module (GeeksforGeeks)](https://www.geeksforgeeks.org/python-os-path-module/)