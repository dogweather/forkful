---
title:                "Å bruke regulære uttrykk"
html_title:           "Fish Shell: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lære å bruke regulære uttrykk er en nyttig ferdighet for alle som jobber med programmering, uansett språk eller plattform. Det er et kraftig verktøy for å søke, filtrere og manipulere tekst på en effektiv måte.

## Slik gjør du det

Å bruke regulære uttrykk i Fish Shell er enkelt og intuitivt. La oss se på noen eksempler:

```Fish Shell
# Søk etter en bestemt streng
grep fish tekst.txt

# Finn alle linjer som inneholder både "fish" og "shell"
grep -e fish -e shell tekst.txt

# Finn linjer som starter med et tall
grep "^[0-9]" tall.txt

# Finn linjer som slutter med et spesifikt tegn
grep "!" tekst.txt
```

Output:

```Fish Shell
fish
This is an example of using fish shell.
12345
Hello world!
```

Her er noen nyttige tips når du bruker regulære uttrykk i Fish Shell:

- Bruk `-i` flagget for å ignorere store og små bokstaver.
- Kombiner `-v` flagget for å søke etter linjer som ikke inneholder mønsteret ditt.
- Du kan også bruke regulære uttrykk i andre kommandoer, som `ls` eller `rename`.

## Dypdykk

Selv om det kan se litt skummelt ut, er det ikke så vanskelig å forstå hvordan regulære uttrykk fungerer. De bruker spesielle tegn og symboler for å definere et mønster du vil søke etter i en tekststreng. Her er noen viktige elementer å huske på når du jobber med regulære uttrykk:

- `.` betyr hvilket som helst tegn.
- `^` betyr starten av en linje.
- `$` betyr slutten av en linje.
- `[]` definerer et sett av mulige tegn.
- `*` betyr at den foregående karakteren kan gjentas 0 eller flere ganger.
- `+` betyr at den foregående karakteren må gjentas minst én gang.

Det er mange flere tegn og symboler som kan brukes i regulære uttrykk, men disse er noen av de vanligste.

## Se også

- [Fish Shell manualen](https://fishshell.com/docs/current/index.html)
- [RegExr](https://regexr.com/) - Et utmerket verktøy for å teste og lære regulære uttrykk.
- [Fish Shell forumet](https://github.com/fish-shell/fish-shell/discussions) - Still spørsmål og få hjelp fra andre Fish Shell-brukere.