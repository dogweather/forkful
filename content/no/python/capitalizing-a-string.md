---
title:                "Å Kapitalisere en Streng"
html_title:           "Python: Å Kapitalisere en Streng"
simple_title:         "Å Kapitalisere en Streng"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å kapitalisere en streng er en vanlig operasjon i programmering som gjør at du kan endre skrivemåten til en tekststreng til stor bokstav i begynnelsen av hvert ord. Dette kan være nyttig for å forbedre lesbarheten og presentasjonen av tekst i et program.

## Hvordan gjøre det

For å kapitalisere en streng i Python, kan du bruke den innebygde funksjonen `capitalize()`. Denne funksjonen tar ikke inn noen argumenter og returnerer den samme strengen med stor forbokstav i begynnelsen. La oss se på et eksempel:

```Python
streng = "dette er en teststreng"
print(streng.capitalize())
```

Eksempelutgang:
```
Dette er en teststreng
```

Du kan også bruke metoden `title()` for å kapitalisere hvert ord i en streng, ikke bare den første bokstaven:

```Python
streng = "gjør DETTE til en TITTEL"
print(streng.title())
```

Eksempelutgang:
```
Gjør Dette Til En Tittel
```

## Dypdykk

Det finnes også andre måter å kapitalisere en streng på i Python, som for eksempel metoden `upper()` som gjør alle bokstavene i strengen til store bokstaver. Det er også mulig å bruke `capitalize()` eller `title()` på en del av en streng ved å bruke indeksering eller slicing. For eksempel:

```Python
streng = "dette er en teststreng"
print(streng[0].upper() + streng[1:])
```

Eksempelutgang:
```
Dette er en teststreng
```

Nå som du vet hvordan du kan kapitalisere en streng i Python på forskjellige måter, kan du begynne å gjøre programmene dine mer leselige og profesjonelle.

## Se også

- [Python dokumentasjon for `capitalize()`](https://docs.python.org/3/library/stdtypes.html#str.capitalize)
- [Python dokumentasjon for `title()`](https://docs.python.org/3/library/stdtypes.html#str.title)
- [Python dokumentasjon for `upper()`](https://docs.python.org/3/library/stdtypes.html#str.upper)