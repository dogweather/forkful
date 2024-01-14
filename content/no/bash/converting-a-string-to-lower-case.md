---
title:                "Bash: Oversettelse av en streng til små bokstaver."
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Konvertering av en streng til små bokstaver kan være nyttig når du jobber med tekstbehandling eller manipulering av data. Det kan også være nødvendig når du samarbeider med andre programmer eller systemer som bruker små bokstaver som standard. Ved å ha en funksjon for konvertering til små bokstaver i bash-programmering, kan du effektivt håndtere og behandle tekstdata.

## Hvordan gjøre det

For å konvertere en streng til små bokstaver i bash, kan du bruke kommandoen "tr". Denne kommandoen endrer tegnene i en streng basert på et gitt sett med regler. I dette tilfellet vil vi bruke regelen "[:upper:]" til å bytte ut alle store bokstaver med tilsvarende små bokstaver.

```Bash
echo "HELLO WORLD" | tr '[:upper:]' '[:lower:]'
```

Dette vil gi følgende output: "hello world"

Du kan også bruke denne kommandoen i kombinasjon med andre kommandoer, som for eksempel å lese en fil og konvertere alle bokstaver til små bokstaver.

```Bash
cat file.txt | tr '[:upper:]' '[:lower:]'
```

## Dypdykk

For å gjøre en mer detaljert konvertering av en streng til små bokstaver, kan du også bruke kommandoen "sed". Dette vil tillate deg å kontrollere nøyaktig hvilke bokstaver som blir konvertert og hvordan.

```Bash
echo "hello world" | sed -e 's/./\L&/g'
```

I dette tilfellet bruker vi "sed" til å endre hvert tegn i strengen til små bokstaver. Dette gjøres ved å bruke "s/" for å erstatte hvert tegn og "\L&/" for å gjøre det om til små bokstaver. Dette vil produsere den samme outputen som i eksemplene over.

## Se også

- [Bash-tråden til omgjøring av store og små bokstaver](https://stackoverflow.com/questions/2264428/how-to-convert-uppercase-letters-to-lowercase) 
- [Tr-kommandoen dokumentasjon](https://www.gnu.org/software/sed/manual/html_node/The-_0022tr_0022-Command.html)
- [Sed-kommandoen dokumentasjon](https://www.gnu.org/software/sed/manual/sed.html)