---
title:                "Fish Shell: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er mange grunner til hvorfor man kanskje vil konvertere en streng til små bokstaver i Fish Shell-programmering. En av de vanligste grunnene er å sikre konsistent datainnsamling eller sammenligning, siden strenger med forskjellige store og små bokstaver kan føre til uforutsette problemer.

## Hvordan

For å konvertere en streng til små bokstaver i Fish Shell, kan du bruke kommandoen "string tolower" etterfulgt av strengen du vil konvertere. Her er et eksempel på hvordan dette kan se ut i praksis:

```Fish Shell
string tolower "HeI på DeG"
```

Dette vil gi følgende utdata:

```Fish Shell
hei på deg
```

Som du kan se, har strengen nå blitt konvertert til kun små bokstaver. Om du ønsker å lagre dette i en variabel for senere bruk, kan du enkelt gjøre det på følgende måte:

```Fish Shell
set lower_string (string tolower "HeI på DeG")
```

## Dypdykk

Å konvertere en streng til små bokstaver kan virke som en enkel oppgave, men det er faktisk en del underliggende prosesser som skjer i bakgrunnen. Når du bruker kommandoen "string tolower" i Fish Shell, bruker den faktisk en funksjon kalt "tolower" som er en del av standardbiblioteket til Fish Shell.

Denne funksjonen tar inn en streng og bruker en algoritme for å analysere hvert enkelt tegn og sjekke om det er en stor bokstav. Hvis det er det, blir det konvertert til en tilsvarende liten bokstav og returnert som utdata. Ved hjelp av denne funksjonen kan du konvertere strenger av forskjellige språk, uansett tegnsett.

## Se også

- [Fish Shell-dokumentasjon om strenger](https://fishshell.com/docs/current/index.html#strings)
- [Fish Shell GitHub-repository](https://github.com/fish-shell/fish-shell)
- [Tutorial for å komme i gang med Fish Shell](https://fishshell.com/docs/current/tutorial.html)