---
title:                "Utdrag av understrenger"
html_title:           "Bash: Utdrag av understrenger"
simple_title:         "Utdrag av understrenger"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

# Bash Programmering: Ekstrahere delstrenger i Bash

## Hva & Hvorfor?
Det å trekke ut delstrenger leser deler av en streng, basert på posisjoner. Dette er nyttig for å behandle informasjon på en mer finjustert måte enn hele strenger tillater.

## Slik gjør du det:
I Bash kan du enkelt trekke ut en delstreng fra en streng. Her er hvordan:

```Bash
tekst='Hei, verden!'
echo ${tekst:4:6}
```

Dette gir utskriften:
```
verden
```

De to tallene etter kolonet representerer startposisjonen og lengden på delstrengen, begge beregnet fra null.

## Dypdykk
Delstrengoperasjonen har vært en del av Bash siden versjon 2.0. Dette betyr at du kan bruke det i nesten hvilket som helst Bash-skript uten bekymring for kompatibilitetsproblemer. 

Det finnes alternativer for å trekke ut delstrenger, som `cut` og `awk`, men ingenting slår bekvemmeligheten og ytelsen til innebygd Bash-strengbehandling.

Implementasjonsdetaljer kan variere mellom forskjellige Bash-versjoner og -miljøer, men grunnleggende fungerer delstrengutvinningsteknikken ved hjelp av Zeichenkettentrennung.

## Se også
For mer informasjon om delstrengbehandling i Bash, sjekk ut følgende ressurser:

1. [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/chap_10.html): En omfattende guide til Bash for nybegynnere.
2. [Bash-manualen](https://www.gnu.org/software/bash/manual/bash.html): Offisiell dokumentasjon fra GNU-prosjektet.
3. [Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/string-manipulation.html): En dypere dykk i avansert Bash-skripting.