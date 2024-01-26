---
title:                "Sette streng til store bokstaver"
html_title:           "Arduino: Sette streng til store bokstaver"
simple_title:         "Sette streng til store bokstaver"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Hva & Hvorfor?)
Kapitalisering av strenger betyr å gjøre første bokstav i hvert ord til en stor bokstav. Programmerere gjør dette for å formatere tekst, for eksempel i titler eller når man skal starte en ny setning.

## How to (Slik gjør du det)
```Bash
#!/bin/bash
# Enkel funksjon for å kapitalisere strenger
capitalize() {
  echo "$1" | awk '{ for(i=1;i<=NF;i++) $i=toupper(substr($i,1,1)) tolower(substr($i,2)); }1'
}

# Bruk funksjonen
capitalize "hei, dette er en test."
```
Output:
```
Hei, Dette Er En Test.
```

## Deep Dive (Dypdykk)
Tidligere var det vanlig å bruke `tr` eller `sed` for tekstmanipulasjon i bash, men disse hadde begrensninger. Med `awk`, et kraftfullt tekstbehandlingsverktøy lansert på slutten av 1970-tallet, er det enkelt å iterere over ord og endre bokstavstørrelsen.

Alternativer til `awk` er å bruke andre programmeringsspråk som Python eller Perl direkte i bash-skriptet, men dette er tyngre og ikke alltid nødvendig.

Når det gjelder implementering, bør `toupper` og `tolower` i `awk` brukes varsomt da de er avhengige av lokalisering (locale), som kan variere i ulike systemer.

## See Also (Se også)
- GNU Awk User's Guide: https://www.gnu.org/software/gawk/manual/gawk.html
- The Single UNIX Specification, Version 2 – Shell & Utilities: https://pubs.opengroup.org/onlinepubs/007908799/catidex.html
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/
