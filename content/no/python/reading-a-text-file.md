---
title:                "Lese en tekstfil"
html_title:           "C#: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å lese en tekstfil betyr å trekke ut og tolke innholdet i filen med programmering. Programmører gjør dette for å manipulere data, for eksempel for å analysere, transformere, presentere eller lagre det på en annen måte.

## Hvordan:

Så, la oss hoppe rett inn. Vi bruker Python innebygde funksjon `open('filename', 'mode')` for å åpne en fil:

```Python
# Åpne en tekstfil
file = open('my_file.txt', 'r')
```

For å lese hele filen, bruk `read()`:

```Python
# Les hele filen
print(file.read())
```

Eller, du kan lese én og én linje med `readline()`, eller alle linjene med `readlines()`:

```Python
# Les én linje
print(file.readline())

# Les alle linjer
print(file.readlines())
```
Husk alltid å lukke filen når du er ferdig:

```Python
# Lukk filen
file.close()
```
Med `with` setningen kan du åpne filen, og Python lukker den automatisk for deg:

```Python
# Åpne og les filen, så lukker Python den for deg
with open('my_file.txt', 'r') as file:
    print(file.read())
```
## Dyp Dykk:

Historisk sett har hver datamaskin sitt eget system for å håndtere filåpning og avlesning. Python skjuler disse forskjellene og gir deg en enkel metode for å lese tekstfiler.

Alternativt kan du bruke `numpy.genfromtxt`, `pandas.read_csv` eller andre biblioteker, spesielt for større datasett eller mer komplekse filformater.

Implementeringsdetaljene bak `open()` og `read()` kan være ganske komplekse, da de må ta hensyn til forskjellige operativsystemer, tegnsett og feilhåndtering.

## Se Også:

* Python Offisielle Dokumentasjon: [Input and Output](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
* GeeksforGeeks Artikkel: [File Handling in Python](https://www.geeksforgeeks.org/file-handling-python/)
* Datacamp Tutorial: [Reading and Writing Files in Python](https://www.datacamp.com/community/tutorials/reading-writing-files-python)