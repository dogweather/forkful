---
title:                "Python: Lesing av en tekstfil"
simple_title:         "Lesing av en tekstfil"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Lesing av tekstfiler er en vanlig oppgave i Python-programmering. Det kan være nyttig når du trenger å analysere store mengder tekstdata, lese konfigurasjonsfiler eller til og med lage enkel tekstbasert brukerinteraksjon. I denne bloggposten vil vi lære deg hvordan du kan lese en tekstfil i Python og svare på vanlige spørsmål om dette emnet.

## Hvordan

For å lese en tekstfil i Python, kan du bruke den innebygde `open()` funksjonen. La oss si at vi har en tekstfil med navnet "mitt_første_fil.txt" som inneholder følgende tekst:

```
Hei, dette er min første tekstfil!

Kos deg med å lære Python!
```

Vi kan bruke følgende kode for å åpne og lese denne filen:

```Python
fil = open("mitt_første_fil.txt", "r") # "r" betyr at vi åpner filen i lesemodus
innhold = fil.read() # Leser hele filen og lagrer den i en variabel
print(innhold) # Skriver ut innholdet i filen
```

Konsollet vil da vise følgende output:

```
Hei, dette er min første tekstfil!

Kos deg med å lære Python!
```

Vi kan også bruke `readlines()` funksjonen for å lese hver linje i filen som en liste:

```Python
fil = open("mitt_første_fil.txt", "r")
linjer = fil.readlines() # Leser hver linje og lagrer dem i en liste
for linje in linjer: # Går igjennom hver linje i listen
  print(linje) # Skriver ut hver linje
```

Output vil da være:

```
Hei, dette er min første tekstfil!

Kos deg med å lære Python!
```

## Dypdykk

Å lese en tekstfil i Python kan gjøres på ulike måter, avhengig av hva slags formål du har og hvilken type data filen inneholder. Du kan for eksempel lese en fil linje for linje ved å bruke en `for`-løkke, eller du kan bruke `with`-blokker for å sørge for at filen blir lukket etter at du er ferdig med å lese den.

Det er også viktig å nevne at enkelte ganger kan det være nødvendig å spesifisere en karakterkoding når du åpner en tekstfil, spesielt hvis den inneholder spesielle karakterer eller tegn fra andre språk.

## Se også

- [Offisiell Python dokumentasjon for tekstfiler](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Stack Overflow thread om å lese tekstfiler i Python](https://stackoverflow.com/questions/1450393/how-do-you-read-a-file-line-by-line-into-a-list)
- [Programiz tutorial om å lese og skrive filer i Python](https://www.programiz.com/python-programming/file-operation)