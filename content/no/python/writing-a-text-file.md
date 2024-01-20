---
title:                "Skriver en tekstfil"
html_title:           "Python: Skriver en tekstfil"
simple_title:         "Skriver en tekstfil"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Å skrive en tekstfil betyr å lagre informasjon på en enkel og strukturert måte på datamaskinen din. Programmerere bruker dette for å lagre data som kan brukes senere, for eksempel resultater fra et program eller konfigurasjonsinnstillinger.

## Slik gjør du det:
```Python
#Åpne en tekstfil for skriving
file = open("filnavn.txt", "w")

#Skriv informasjon
file.write("Hei, dette er en tekstfil som inneholder informasjon.")

#Lukk filen
file.close()
```

## Dypdykk:
Skriftlig informasjon har vært en del av menneskeheten i århundrer, og det er ikke annerledes i programmering. Noen alternativer til å skrive tekstfiler inkluderer å bruke en database eller en annen form for datalagring. Når du skriver en tekstfil, må du være oppmerksom på filnavnet, modusen (som "w" for å skrive) og språket du bruker for å skrive filen.

## Se også:
- [Tutorialspoint - Writing Files in Python](https://www.tutorialspoint.com/python/file_write.htm)