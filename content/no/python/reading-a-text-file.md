---
title:                "Lese en tekstfil"
html_title:           "Python: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lese en tekstfil kan være nyttig i mange situasjoner. Det kan for eksempel være for å analysere data eller behandle store mengder informasjon.

## Hvordan

For å lese en tekstfil i Python, åpner vi først filen ved hjelp av "open" funksjonen. Vi må også spesifisere om vi vil lese filen ("r"), skrive til den ("w") eller legge til ny informasjon ("a"). Deretter kan vi bruke "read()" funksjonen for å lese innholdet i filen.

```Python
f = open("tekstfil.txt", "r")
print(f.read())
```

Dette vil skrive ut innholdet i tekstfilen til konsollen. Vi kan også lese innholdet linje for linje ved bruk av "readline()" funksjonen.

```Python
f = open("tekstfil.txt", "r")
print(f.readline())
```

For å lukke filen når vi er ferdig med å lese den, kan vi bruke "close()" funksjonen.

```Python
f.close()
```

## Dypdykk

Når vi leser en tekstfil i Python, leser vi den som en tekststreng. Derfor må vi være nøye med å håndtere forskjeller i formatering og spesialtegn. En annen ting å være oppmerksom på er at hvis filen er veldig stor, kan det være nødvendig å lese den innholdet i den bit for bit for å unngå å overbelaste minnet.

Men vi trenger ikke å bekymre oss for mye om disse detaljene hvis vi bare skal lese en enkel tekstfil.

## Se også

- [Python sin offisielle dokumentasjon for å lese og skrive til filer](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [En tutorial om å arbeide med tekstfiler i Python](https://www.datacamp.com/community/tutorials/reading-writing-files-python)