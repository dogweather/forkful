---
title:    "Python: Å skrive en tekstfil"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive en tekstfil er en vanlig oppgave i programmering, og det er nyttig å vite hvordan man gjør det for å kunne lagre og behandle data. Dette kan også være nyttig når man jobber med større programmer der man ønsker å lagre informasjon i separate filer.

## Hvordan

For å skrive en tekstfil i Python, kan man bruke den innebygde funksjonen `open()`. Denne funksjonen tar to argumenter, filnavnet og modusen hvor man sier at man ønsker å skrive til filen (i dette tilfellet "w" for "write").

```python
f = open("tekstfil.txt", "w")
```
Man kan da skrive til filen ved å bruke `write()`-metoden, og passere inn teksten man vil skrive.

```python
f.write("Dette er en tekst som blir skrevet til filen.")
```

Til slutt må man huske å lukke filen ved å bruke `close()`-metoden, ellers vil endringene ikke bli lagret.

```python
f.close()
```

Hvis man ønsker å skrive flere linjer til filen, må man passe på å legge til en linjeskift `\n` etter hver linje.

```python
f.write("Dette er linje 1.\n")
f.write("Dette er linje 2.")
```

## Dypdykk

Det finnes flere forskjellige moduser man kan velge når man åpner en fil med `open()`, for eksempel "a" for "append" som legger til tekst i slutten av filen uten å overskrive eksisterende data. Man kan også lese og skrive til samme fil ved å bruke "r+" eller "w+" som moduser.

Når man skriver til en fil, blir teksten lagret som en sekvens av bytes. For å lese teksten som vanlig tekst, kan man bruke `encoding`-parameteret og sette det til den ønskede kodingen (for eksempel "utf-8").

Det er også viktig å merke seg at hvis man åpner en fil med "w" eller "w+" som modus, vil eventuelle eksisterende data i filen bli slettet uten advarsel. Derfor er det viktig å være forsiktig når man bruker disse modusene.

## Se også

- [Python dokumentasjon](https://docs.python.org/no/3/tutorial/inputoutput.html#reading-and-writing-files)
- [W3Schools Python file handling tutorial (engelsk)](https://www.w3schools.com/python/python_file_handling.asp)
- [Real Python tutorial on writing and reading files (engelsk)](https://realpython.com/read-write-files-python/)