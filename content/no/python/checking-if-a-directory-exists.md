---
title:                "Python: Sjekke om en mappe eksisterer"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange ganger når vi skriver programmer, vil vi sannsynligvis arbeide med forskjellige filer og mapper. Noen ganger trenger vi å sjekke om en bestemt mappe eksisterer før vi kan fortsette med resten av koden vår. Dette kan være spesielt nyttig hvis vi prøver å navigere i en bestemt mappe, kopiere filer eller opprette nye mapper.

## Hvordan

For å sjekke om en mappe eksisterer i Python, kan vi bruke funksjonen `path.exists()` fra `os` modulen. Dette vil returnere `True` hvis mappen eksisterer og `False` hvis den ikke gjør det.

```Python
import os

# Sjekk om mappen "Dokumenter" eksisterer
if os.path.exists("Dokumenter"):
    print("Mappen eksisterer")
else:
    print("Mappen eksisterer ikke")
```

Output:

```
Mappen eksisterer
```

En annen måte å sjekke eksistensen av en mappe er ved å bruke `path.isdir()` funksjonen. Denne vil returnere `True` hvis stien peker på en mappe og `False` hvis det er en fil.

```Python
import os

# Sjekk om mappen "Bilder" er en mappe
if os.path.isdir("Bilder"):
    print("Bilder er en mappe")
else:
    print("Bilder er ikke en mappe")
```

Output:

```
Bilder er en mappe
```

Vi kan også bruke `path.join()` funksjonen til å kombinere to stier og dermed sjekke om en mappe eksisterer i en bestemt undermappe. For eksempel:

```Python
import os

# Kombiner stien til "Dokumenter" med "mine_prosjekter" og sjekk om den eksisterer
if os.path.exists(os.path.join("Dokumenter", "mine_prosjekter")):
    print("Undermappen eksisterer")
else:
    print("Undermappen eksisterer ikke")
```

Output:

```
Undermappen eksisterer ikke
```

## Dypdykk

Når vi sjekker om en mappe eksisterer, kan det være lurt å vurdere hvilke faktorer som kan påvirke resultatet. For eksempel kan filrettigheter, stienespesifiseringer og til og med operativsystemet vårt ha en rolle i denne prosessen.

Det kan også være nyttig å ta hensyn til unntak eller feil som kan oppstå når vi prøver å sjekke en mappes eksistens. Det er viktig å håndtere disse unntakene på en riktig måte for å unngå krasj i programmet vårt.

## Se også

- Dokumentasjon for `os` modulen: https://docs.python.org/3/library/os.html
- Mer om å håndtere filer og mapper i Python: https://www.w3schools.com/python/python_file_handling.asp
- Utforske ulike måter å sjekke eksistensen av en mappe i Python: https://stackabuse.com/python-check-if-a-file-or-directory-exists/