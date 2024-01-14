---
title:                "Bash: Konvertere en streng til små bokstaver."
simple_title:         "Konvertere en streng til små bokstaver."
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange ganger i Bash-programmering kan det hende du trenger å konvertere en streng til små bokstaver. Dette kan være for å sammenligne input fra brukeren med forventede verdier, eller for å sikre at dataen din er i en ensartet form.

## Hvordan

Det er flere måter å konvertere en streng til små bokstaver i Bash. En måte er å bruke `tr` kommandoen, som står for "translate". Denne kommandoen lar deg erstatte en sekvens av karakterer med en annen sekvens. For å konvertere en streng til små bokstaver med `tr`, kan du gjøre følgende:

```
Bash
# Lag en variabel med tekst
original_tekst="HEI, JEG ER EN STRENG"

# Bruk 'tr' kommandoen for å konvertere til små bokstaver
konvertert_tekst=$(echo $original_tekst | tr '[:upper:]' '[:lower:]')

# Skriv ut den konverterte teksten
echo $konvertert_tekst
```

Output:

```
hei, jeg er en streng
```

Det er også mulig å konvertere en streng til små bokstaver ved å bruke `awk` kommandoen og dens innebygde `tolower` funksjon. Dette kan se slik ut:

```
Bash
# Lag en variabel med tekst
original_tekst="HEI, JEG ER EN STRENG"

# Bruk 'awk' kommandoen for å konvertere til små bokstaver
konvertert_tekst=$(echo $original_tekst | awk '{print tolower($0)}')

# Skriv ut den konverterte teksten
echo $konvertert_tekst
```

Output:

```
hei, jeg er en streng
```

## Dypdykk

Både `tr` og `awk` kommandoene kan også brukes til å konvertere en streng til store bokstaver ved å erstatte `[:lower:]` med `[:upper:]`. Du kan også bruke flere forskjellige sett med spesialtegn i `tr` kommandoen for å konvertere andre typer bokstaver. For eksempel, `tr '[åæÅÆ]' '[aaAA]'` vil konvertere alle norske bokstaver til engelsk form.

Det er også mulig å bruke regulære uttrykk i `awk` kommandoen for mer avansert tekstbehandling. Det kan du lese mer om i `man` siden for `awk`.

## Se Også

- [Bash `tr` kommandoen](https://linux.die.net/man/1/tr)
- [Bash `awk` kommandoen](https://www.gnu.org/software/gawk/manual/gawk.html)
- [Bash regulære uttrykk](https://www.gnu.org/software/gawk/manual/html_node/Regexp.html#Regexp)