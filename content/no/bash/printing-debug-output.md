---
title:                "Bash: Utskrift av feilsøkingsutdata"
programming_language: "Bash"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Når man skriver Bash-programmer, er det ofte at man kommer over feil og problemer som må løses. I slike situasjoner kan det være nyttig å ha en måte å sjekke hva som foregår i programmet for å finne ut hva som forårsaker feilen. Dette er hvor å printe debug output kommer inn i bildet.

## Hvordan gjøre det

Det er flere måter å printe debug output når man skriver Bash-programmer. En enkel metode er å bruke `echo` kommandoen og printe ut variabler eller beskrivelser av hva som skjer i programmet. Her er et eksempel på hvordan man kan printe ut en variabel:

```Bash
NAME="Per"
echo "Navnet er $NAME"
```

Dette vil gi følgende output:

```Bash
Navnet er Per
```

Man kan også bruke kommandoen `set -x` for å printe ut hele kommandolinjen og ønsket output til skjermen. Dette er spesielt nyttig når man ønsker å se alle steg i programmet og sjekke hva som går galt. Her er et eksempel på hvordan dette kan se ut:

```Bash
set -x
NAME="Per"
echo "Navnet er $NAME"
```

Dette vil gi følgende output:

```Bash
+ NAME="Per"
+ echo "Navnet er $NAME"
Navnet er Per
```

## Dypdykk

Å printe debug output er en nyttig måte å finne ut hva som skjer i et Bash-program. Det kan også være lurt å bruke forskjellige nivåer av debug output, der man printer ut mer detaljert informasjon hvis feilene man opplever er mer komplekse. Dette kan gjøres ved å bruke `set` kommandoen med forskjellige flags, som `-v` for å printe ut variablene som en blir brukt, og `-x` for å printe ut hele kommandolinjen.

Det er også mulig å lagre debug output i en fil ved å bruke `>>` operator etter kommandoen man ønsker å printe ut. Dette vil føre til at output blir lagt til i en fil istedenfor å bare bli vist på skjermen. Dette kan være nyttig hvis man trenger å se gjennom mye output for å finne ut hva som forårsaker en feil.

## Se også

- [Official Bash Documentation](https://www.gnu.org/software/bash/)
- [Debugging shell scripts with bash -x](https://www.linux.com/news/debugging-shell-scripts-bash-x/)