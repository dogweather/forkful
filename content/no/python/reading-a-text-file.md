---
title:    "Python: Leser en tekstfil"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Lurer du på hvordan du kan lese innholdet i en tekstfil ved hjelp av Python? Da er du på rett sted! Denne bloggposten vil gi deg en enkel og grei forklaring på hvorfor det er nyttig å kunne lese tekstfiler i Python, samt hvordan du kan gjøre dette trinn for trinn.

## Hvordan du kan lese en tekstfil med Python

Når du jobber med programmering, er det ofte nødvendig å hente inn data fra ulike kilder. En vanlig måte å gjøre dette på er ved å lese en tekstfil. Med Python kan du enkelt lese innholdet i en tekstfil og deretter behandle det som du ønsker.

For å lese en tekstfil i Python må du først åpne filen ved hjelp av funksjonen `open()` og spesifisere filnavnet og ønsket modus for lesing. Deretter kan du bruke en `for`-løkke for å lese linje for linje i filen og lagre dette i en variabel. Til slutt kan du skrive ut innholdet ved hjelp av `print()`-funksjonen.

```Python
with open("tekstfil.txt", "r") as fil:
    for linje in fil:
        print(linje)
```

Dette vil skrive ut hele innholdet i filen `tekstfil.txt`.

## En dypere forklaring

Når vi leser en tekstfil ved hjelp av Python, behandler vi den som en tekststreng. Dette betyr at vi kan utføre ulike operasjoner som å splitte teksten i forskjellige deler, søke etter spesifikke ord eller setninger, eller endre innholdet før vi skriver det ut igjen. Ved å lære hvordan man leser tekstfiler i Python, åpner man også opp for muligheten til å jobbe med store mengder data på en effektiv måte.

## Se også

For å lære mer om å lese og behandle tekstfiler med Python, kan du sjekke ut følgende ressurser:

- [Offisiell Python-dokumentasjon for tekstfiler](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [En interaktiv tutorial om å lese og skrive filer i Python](https://www.learnpython.org/en/Reading_and_Writing_Files)
- [En tutorial om å behandle tekstfiler med Python](https://realpython.com/read-write-files-python/)
- [En video som viser hvordan man kan lese tekstfiler i Python](https://www.youtube.com/watch?v=N-rFanWsi58)