---
title:                "Gjøre om en streng til store bokstaver"
html_title:           "Bash: Gjøre om en streng til store bokstaver"
simple_title:         "Gjøre om en streng til store bokstaver"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?

Store bokstaver i en streng betyr å endre alle tegnene i strengen til store bokstaver. Dette er nyttig for programmører for å gjøre datauniforme, lettere for brukeren å lese, eller for å oppfylle visse systemkrav.

## Slik gjør du:

I Bash (siste versjon), kan du bruke innebygd `tr` kommando for å gjøre om strenger til store bokstaver. Her er et eksempel:

```Bash
tekst="jeg liker å kode"
echo "${tekst}" | tr '[:lower:]' '[:upper:]'
```

Output:

```Bash
JEG LIKER Å KODE
```

## Dypdykk

Historisk sett, var store bokstaver de første som ble implementert i datamaskiner på grunn av deres bruk i telegrafsystemer. 

Alternativt kan du bruke `awk` eller `sed` kommandoene til å endre strengene til store bokstaver. Detaljer om implementering kan variere avhengig av hvilken versjon av Bash du bruker, men `tr` funksjonen er vanligvis den mest universelle og enkle å bruke.

```Bash
echo "${tekst}" | awk '{print toupper($0)}'
echo "${tekst}" | sed -e 's/\(.*\)/\U\1/'
```

## Se Også

1. [Bash Handbook](https://github.com/denysdovhan/bash-handbook)
2. [Advanced Bash-Scripting Guide](http://www.tldp.org/LDP/abs/html/)
3. [GNU Bash Manual](https://www.gnu.org/software/bash/manual/bash.html)