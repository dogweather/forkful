---
title:    "Python: Store bokstaver i en streng"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Når man programmerer, er det ofte nødvendig å manipulere og behandle tekststrenger på ulike måter. En vanlig oppgave er å endre bokstavene i en tekststreng slik at de blir enten store eller små. Dette kan være særlig nyttig hvis man for eksempel ønsker å formatere tekst eller utføre en søkefunksjon.

## Hvordan

For å gjøre dette i Python, kan du bruke innebygde funksjoner som 'upper()' og 'lower()'. Men hvis du vil bare vil endre den første bokstaven i hver ord i en tekststreng, kan du bruke 'capitalize()' funksjonen. Se eksempelet nedenfor:

```Python
my_string = "dette er en tekststreng"
print(my_string.capitalize())
```

Dette vil resultere i outputen "Dette er en tekststreng". Som du kan se, er bare den første bokstaven i hvert ord endret til store bokstaver.

## Dypdykk

Det er viktig å merke seg at 'capitalize()' funksjonen kun endrer den første bokstaven i en tekststreng og ingen andre bokstaver. Derfor kan det være lurt å kombinere denne funksjonen med andre for å oppnå ønsket resultat. For eksempel kan du bruke 'replace()' funksjonen til å erstatte en bestemt bokstav eller ord i tekstrengen før du bruker 'capitalize()' for å gjøre den første bokstaven stor.

## Se også

- [Dokumentasjon for 'capitalize()' funksjonen](https://docs.python.org/3/library/stdtypes.html#str.capitalize)
- [Mer informasjon om strenger i Python](https://realpython.com/python-strings/)