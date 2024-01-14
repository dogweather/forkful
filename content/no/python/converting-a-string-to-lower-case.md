---
title:                "Python: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er mange grunner til å ønske å konvertere en streng til små bokstaver i Python. Det kan være for å gjøre en streng sammenlignbar med en annen, for å lage en mer lesbar utskrift, eller for å standardisere input fra brukeren. Uansett årsak, å kunne konvertere en streng til små bokstaver er en nyttig ferdighet for enhver Python-programmerer.

## Hvordan

Konvertere en streng til små bokstaver i Python er enkelt. Ved hjelp av ```lower()``` funksjonen kan du enkelt gjøre dette.

```Python
streng = "DETTE ER EN STRENG"
print(streng.lower())
```

Dette vil skrive ut ```"dette er en streng"```, med alle bokstavene i små bokstaver. Du kan også lagre den nye strengen i en variabel, hvis du ønsker det.

```Python
streng = "Dette er en streng"
liten_streng = streng.lower()
print(liten_streng)
```

Du kan også bruke ```lower()``` funksjonen på variabelnavnet direkte, uten å lagre den nye strengen i en egen variabel.

```Python
streng = "Dette er en streng"
print(streng.lower())
```

Output vil fortsatt være ```"dette er en streng"```.

Det er viktig å huske at ```lower()``` funksjonen kun konverterer alfabetiske bokstaver til små bokstaver. Tall, symboler og mellomrom vil ikke bli påvirket.

## Dypdykk

I bunn og grunn bruker ```lower()``` funksjonen ```str.lower()``` metoden. Dette betyr at metoden ```lower()``` er spesifikk for strenger og kan ikke brukes på andre datatyper. 

Det finnes også en lignende funksjon kalt ```casefold()```, som tar hensyn til spesielle bokstaver i ulike språk. Dette kan være nyttig hvis du jobber med flerspråklige tekster.

Det er også verdt å merke seg at strenger i Python er uforanderlige. Dette betyr at du ikke kan endre bokstavene i en streng direkte. Derfor vil ```lower()``` funksjonen returnere en ny streng, og ikke endre den opprinnelige strengen.

## Se også

- [Python's offisielle dokumentasjon for ```lower()``` funksjonen](https://docs.python.org/3/library/stdtypes.html#str.lower)
- [Mer informasjon om strenger i Python](https://www.w3schools.com/python/python_strings.asp)
- [En introduksjon til Python-programmering på norsk](https://www.informatikk-mooc.no/course/python)