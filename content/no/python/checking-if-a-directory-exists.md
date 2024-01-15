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

## Hvorfor

Det er viktig å sjekke om en mappe eksisterer i koden din for å unngå feil og sikre at programmet ditt fungerer som det skal. Ved å sjekke om en mappe eksisterer, kan du også ta ulike handlinger basert på om mappen allerede finnes eller ikke.

## Slik gjør du det

For å sjekke om en mappe eksisterer i Python, kan du bruke funksjonen `os.path.exists()`. Dette gjør at du kan kontrollere om en mappe finnes i et gitt sted på datamaskinen din, og returnerer en sann eller usann verdi basert på eksistensen av mappen. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```Python
import os

if os.path.exists('mappe_navn'):
    print('Mappen finnes')
else:
    print('Mappen finnes ikke')
```

I dette eksempelet sjekker vi om en mappe med navnet "mappe_navn" finnes i den nåværende arbeidsmappa vår. Dersom mappen finnes, vil programmet skrive ut "Mappen finnes", ellers vil det skrive ut "Mappen finnes ikke".

## Dypdykk

Det finnes også flere måter å sjekke om en mappe eksisterer i Python på. En annen måte er å bruke `os.path.isdir()`-funksjonen, som sjekker om det gitte stedet er en mappe eller ikke. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```Python
import os

if os.path.isdir('mappe_navn'):
    print('Dette er en mappe')
else:
    print('Dette er ikke en mappe')
```

En annen metode er å bruke `os.path.isfile()`-funksjonen, som sjekker om det gitte stedet er en fil eller ikke. Denne kan være nyttig hvis du ønsker å sjekke om en spesifikk fil eksisterer i en mappe. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```Python
import os

if os.path.isfile('fil_navn.txt'):
    print('Filen finnes')
else:
    print('Filen finnes ikke')
```

Det finnes også mange andre nyttige metoder for å sjekke om en mappe eksisterer i Python, som for eksempel `os.access()` for å kontrollere om du har tilgang til å lese, skrive eller kjøre filer fra den gitte mappen.

## Se også

- [Dokumentasjon for os.path-modulen](https://docs.python.org/3/library/os.path.html)
- [Tutorial om filbehandling i Python](https://realpython.com/working-with-files-in-python/)
- [Stack Overflow-tråd om å sjekke om en mappe eksisterer](https://stackoverflow.com/questions/8933237/how-to-find-if-directory-exists-in-python)