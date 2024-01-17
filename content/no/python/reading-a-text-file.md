---
title:                "Leser en tekstfil"
html_title:           "Python: Leser en tekstfil"
simple_title:         "Leser en tekstfil"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?
Lesing av en tekstfil er en måte for programmerere å få tilgang til informasjon som er lagret i en fil. Programmere bruker dette for å lese og behandle data uten å måtte skrive inn all informasjon manuelt.

## Hvordan:
Du kan lese en tekstfil ved hjelp av ```Python```'s innebygde ```open()``` funksjon. Denne funksjonen tar inn to argumenter, filnavnet og modusen du ønsker å åpne filen i. For å lese en tekstfil, bruk modusen ```r```. Her er et eksempel på å lese en tekstfil og skrive ut innholdet:

```Python
f = open("tekstfil.txt", "r")
print(f.read())
```

Dette vil skrive ut hele innholdet i tekstfilen "tekstfil.txt".

Du kan også lese en tekstfil linje for linje ved hjelp av en ```for``` løkke. Her er et eksempel som leser en tekstfil og skriver ut hver linje som et element i en liste:

```Python
f = open("tekstfil.txt", "r")
lines = []
for line in f:
  lines.append(line)
print(lines)
```

Output:

```Python
["Dette er linje 1\n", "Dette er linje 2\n", "Dette er linje 3\n"]
```

## Dypdykk:
Lesing av tekstfiler har vært en grunnleggende funksjon i programmering siden begynnelsen. Det er ofte brukt for å lagre og behandle data, som for eksempel tekstfiler med informasjon om brukere eller produkter. Alternativet til å lese en tekstfil er å bruke en database, men dette kan være mer komplekst og kreve mer arbeid.

Når du leser en tekstfil, er det viktig å huske på å lukke filen etterpå ved hjelp av ```close()``` funksjonen. Dette sikrer at all brukerdata blir lagret før filen lukkes. Det finnes også flere moduser for å åpne en tekstfil, som for eksempel ```w``` for å skrive til en fil, ```a``` for å legge til informasjon i en eksisterende fil, og ```x``` for å opprette en ny fil for skriving.

## Se også:
- [Python Dokumentasjon for åpning av filer](https://docs.python.org/3/library/functions.html#open)
- [W3Schools tutorial for å lese tekstfiler i Python](https://www.w3schools.com/python/python_file_handling.asp)