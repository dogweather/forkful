---
title:                "Python: Lese en tekstfil"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Hvorfor

Å lese tekstdokumenter er en viktig ferdighet for enhver programmerer. Det lar deg få tilgang til og analysere store mengder data, som kan være avgjørende for å løse programmeringsproblemer. I denne blogginnlegget, vil vi utforske hvordan du leser tekstfiler ved hjelp av Python.

# Hvordan

Det første trinnet for å lese en tekstfil i Python er å åpne filen. Dette gjøres ved hjelp av "open()" funksjonen, og tar to argumenter: filbanen og "modus". Modusen kan være "r" for å lese eller "w" for å skrive til filen. La oss si at vi ønsker å lese en tekstfil med navnet "tekstfil.txt" som ligger på skrivebordet vårt. Koden vil se slik ut:

```Python
fil = open('skrivebord/tekstfil.txt', 'r')
```

Nå som vi har åpnet filen, kan vi lese innholdet ved å bruke "read()" funksjonen. Dette vil returnere en streng med hele innholdet i filen. La oss nå skrive ut innholdet i konsollen:

```Python
innhold = fil.read()
print(innhold)
```

Output vil se slik ut:

```
Dette er en tekstfil.
Det er en fin dag i dag.
Jeg liker å kode i Python.
```

Vi kan også lese filen linje for linje ved å bruke "readline()" funksjonen. Dette vil returnere én linje av gangen. La oss prøve å skrive ut de første to linjene:

```Python
print(fil.readline())
print(fil.readline())
```

Output vil se slik ut:

```
Dette er en tekstfil.
Det er en fin dag i dag.
```

# Dypdykk

Det er også andre nyttige metoder for å lese filer i Python, som for eksempel "readlines()" som vil returnere en liste med alle linjene i filen, og "seek()" som lar deg navigere gjennom filen ved å bevege deg til bestemte byte-posisjoner. Du kan også endre modusen for å skrive til filen ved å bruke "write()" funksjonen.

En annen viktig ting å huske på når du leser en tekstfil er å lukke den når du er ferdig. Dette gjøres ved å bruke "close()" funksjonen. Det er viktig å lukke filen for å frigjøre systemressurser og forhindre uventet oppførsel.

# Se også

- [Python dokumentasjon for å lese og skrive filer](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [En guide til å lese og skrive filer i Python](https://realpython.com/read-write-files-python/)
- [En tutorial for å lære mer om filbehandling i Python](https://www.geeksforgeeks.org/file-handling-python/)