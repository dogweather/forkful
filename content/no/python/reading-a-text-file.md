---
title:    "Python: Lese en tekstfil"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Hvorfor

Det å lese tekstfiler er en viktig del av programmering, spesielt for å få tilgang til og behandle store mengder data. Det kan også være nyttig når man ønsker å lage automatiserte oppgaver eller tilpasse data på en spesifikk måte.

## Hvordan lese en tekstfil i Python

For å lese en tekstfil i Python må man først åpne filen ved å bruke `open()` funksjonen. Deretter kan man bruke `read()` funksjonen for å lese innholdet i filen og lagre det i en variabel. Se eksempelet nedenfor:

```Python
file = open("navn.txt", "r")
data = file.read()
print(data)
```

Denne koden vil åpne filen "navn.txt" og skrive ut hele innholdet i filen til konsollen. Det er også mulig å bruke `readline()` funksjonen for å lese en linje av gangen, eller `readlines()` for å lagre hvert linje som et element i en liste.

Det er viktig å huske å lukke filen etter at man er ferdig med å lese den, ved å bruke `close()` funksjonen.

## Dypdykk i lesing av tekstfiler

Når man leser en tekstfil i Python, returneres alltid en string. Dette betyr at man må konvertere dataen til ønsket format, for eksempel ved å bruke `int()` eller `float()` funksjonene.

Det er også mulig å lese innholdet i en tekstfil linje for linje ved å bruke en `for` loop. Se eksempelet nedenfor:

```Python
file = open("navn.txt", "r")
for line in file:
  print(line.strip())
```

Her vil hver linje i filen bli skrevet ut, uten å inkludere linjeskift.

Når man jobber med store tekstfiler, kan det være mer effektivt å lese én linje av gangen i stedet for hele filen samtidig. Dette gjøres ved å bruke `next()` funksjonen. Se eksempelet nedenfor:

```Python
file = open("navn.txt", "r")
line = next(file)
print(line)
```

Denne koden vil lagre den første linjen i filen som en string i variabelen `line`.

## Se også

- [Python dokumentasjon: Lesing og skriving av filer](https://docs.python.org/no/3/tutorial/inputoutput.html)
- [Real Python: How to Read and Write Files in Python](https://realpython.com/read-write-files-python/)
- [Programiz: Python File Handling](https://www.programiz.com/python-programming/file-operation)