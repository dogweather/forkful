---
title:    "Python: Konverterer en streng til små bokstaver"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Hvorfor 
I denne bloggposten vil vi dykke inn i hvordan du kan konvertere en streng til små bokstaver i Python. Dette kan være nyttig for å gjøre søk eller sammenligninger av tekststrenger mer effektivt, da det eliminerer forskjeller i store og små bokstaver.

## Slik Gjør Du
Python har en innebygd funksjon kalt `lower()` som kan brukes til å konvertere en streng til små bokstaver. La oss ta en titt på et eksempel:

```Python
streng = "Hei på deg!"
print(streng.lower())
```
Dette vil gi følgende utdata:
```
hei på deg!
```

## Dykk Dypere
Det er viktig å være klar over at `lower()`-funksjonen ikke gjør endringer i selve strengen, men heller returnerer en kopi med de konverterte små bokstavene. Dette betyr at hvis du ønsker å lagre den konverterte versjonen, må du tilordne den til en ny variabel.

I tillegg kan det være nyttig å vite at `lower()`-funksjonen også kan brukes på individuelle bokstaver, ikke bare hele strenger. Dette kan gjøres ved å bruke indeksering eller slicing for å hente ut og konvertere enkeltbokstaver.

## Se Også 
Her er noen nyttige ressurser for å lære mer om strenger og ulike metoder for manipulering av tekst i Python:

- [Offisiell Django-dokumentasjon for `lower()`-funksjonen](https://docs.python.org/3/library/stdtypes.html#str.lower)
- [En guide til strenger i Python](https://realpython.com/python-strings/)
- [En tutorial om indeksering og slicing i Python](https://www.datacamp.com/community/tutorials/advanced-indexing-slicing-techniques-python)