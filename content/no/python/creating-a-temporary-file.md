---
title:    "Python: Oppretting av midlertidig fil"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Hvorfor

Å lage en midlertidig fil er en praktisk måte å håndtere data på når du jobber med Python-programmering. Det er spesielt nyttig når du for eksempel trenger å behandle store datamengder, eller når programmet ditt krever midlertidig lagring av informasjon.

## Hvordan

Det er enkelt å opprette en midlertidig fil i Python. Først må vi importere "tempfile" biblioteket:

```Python
import tempfile 
```

Deretter kan vi bruke "tempfile.NamedTemporaryFile()" funksjonen til å opprette en midlertidig fil:

```Python
with tempfile.NamedTemporaryFile() as temp:
    # Gjør operasjoner med den midlertidige filen her
```

I kodelinjene over opprettes filen automatisk, og "with" statement sørger for at filen blir slettet etter at den ikke lenger er i bruk. Inne i "with" blokken kan du gjøre operasjoner med filen, for eksempel skrive eller lese data.

Det er også mulig å spesifisere en bestemt plassering for den midlertidige filen ved hjelp av "dir" parameteren:

```Python
with tempfile.NamedTemporaryFile(dir="C:/temp") as temp:
    # Gjør operasjoner med den midlertidige filen her
```

Du kan også velge å ikke slette filen automatisk ved å sette "delete" parameteren til False. Dette kan være nyttig hvis du trenger å bruke filen etter at programmet er avsluttet.

## Dypdykk

Når vi oppretter en midlertidig fil, lagres den i det angitte temporære området. Hvis du ikke har spesifisert en plassering, vil filen bli lagret i det system-baserte temporære området. Dette området vil variere avhengig av hvilken plattform du bruker.

Det er også verdt å merke seg at den midlertidige filen ikke vil være synlig for andre programmer på datamaskinen din ettersom den ikke har et synlig filnavn. Dette er grunnen til at den midlertidige filen vanligvis slettes automatisk når programmet er avsluttet eller når "with" blokken er ferdig.

## Se også

- [tempfile dokumentasjon](https://docs.python.org/3/library/tempfile.html)
- [Guide til Python ephemeral objekter](https://realpython.com/python-ephemeral-objects/)
- [Hva er midlertidige filer og hvordan de fungerer?](https://www.howtogeek.com/410679/what-are-temporary-files-and-how-does-one-work/)