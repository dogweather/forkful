---
title:                "Python: Å skrive en tekstfil"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor
Det er flere grunner til å skrive en tekstfil i Python. Det kan være for å lagre data som skal brukes senere, eller for å lage en rapport eller dokumentasjon for et prosjekt.

## Hvordan
Det finnes flere måter å skrive en tekstfil i Python på, men en enkel måte er å bruke `open()` funksjonen. Følgende kode viser hvordan du kan lage en tekstfil og skrive innhold til den:

```python
# Åpne en fil med navnet "min_fil.txt" i skrivemodus
with open("min_fil.txt", "w") as fil:
  # Skriv innhold til filen
  fil.write("Dette er en tekstfil laget med Python!")
```

Etter å ha kjørt koden, vil en ny tekstfil med navnet "min_fil.txt" bli opprettet med innholdet "Dette er en tekstfil laget med Python!".

## Dypdykk
Når du åpner en fil med `open()` funksjonen, må du spesifisere om du vil åpne den i lese-, skrive- eller tilleggsmodus. Dette bestemmes av den andre parameteren i `open()` funksjonen. For eksempel, hvis du vil åpne en fil for å legge til nytt innhold, kan du bruke "a" som parameter istedenfor "w".

Det finnes også flere metoder for å skrive innhold til en fil. `write()` er den mest grunnleggende metoden, men det finnes også `writelines()` som lar deg skrive flere linjer på en gang og `print()` som lar deg formatere og skrive ut variabler.

Det er viktig å huske å lukke filen etter at du er ferdig med å jobbe med den, ved å bruke `close()` funksjonen. Men det kan også være nyttig å bruke `with` kontekst-managers for å sørge for at filen blir lukket automatisk når du er ferdig med å bruke den.

## Se også
- [Python dokumentasjon for filer](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [W3Schools tutorial om å jobbe med filer i Python](https://www.w3schools.com/python/python_file_handling.asp)
- [Django tutorial om hvordan å lese og skrive filer i Python](https://docs.djangoproject.com/en/3.1/topics/files/)