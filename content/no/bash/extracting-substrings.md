---
title:    "Bash: Uttrekking av substringer"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Hvorfor
Noen ganger når vi jobber med tekstbehandlingsoppgaver, trenger vi å hente ut en del av en streng, også kalt en substring. Dette kan være nyttig for å filtrere og analysere data, eller for å lage mer komplekse kommandolinjefunksjoner. Uansett hva årsaken er, kan det å kunne utvinne substrings være en nyttig ferdighet å ha i Bash-programmering.

## Hvordan å utvinne substrings i Bash
For å utvinne substrings i Bash, kan vi bruke kommandoen `cut`. Denne kommandoen tillater oss å velge en del av en streng basert på en bestemt separator, eller skillelinje. La oss se på et eksempel:

```Bash
streng="Hei, verden!"
echo ${streng:4:6}
```
Denne kodelinjen vil utskrive "verden", siden vi har valgt å ekstrahere en del av strengen fra posisjon 4 (inkludert) til posisjon 6 (ekskludert).

Et annet alternativ for å utvinne substrings er å bruke `grep` kommandoen. Dette kan være nyttig hvis vi leter etter en bestemt del av en streng basert på et søkeord eller mønster. Her er et eksempel:

```Bash
streng="En tekststreng for å demonstrere grep kommandoen"
grep 'demonstrere' <<< $streng
```

Dette vil utskrive "En tekststreng for å demonstrere grep kommandoen", siden vi har brukt grep til å finne og utvinne strengen som inneholder ordet "demonstrere".

## Dypdykk
Det finnes også andre kommandoer og metoder for å utvinne substrings i Bash, som `sed` og `awk`. Det viktigste er å forstå konseptet med å velge en del av en streng basert på en bestemt separator eller et mønster. Det er også verdt å merke seg at Bash støtter regulære uttrykk, som kan være nyttige for å finne og ekstrahere mer komplekse substrings.

## Se også
- [BashGuide - Extracting substrings](https://linuxcommand.org/lc3_adv_substrings.php)
- [Bash Reference Manual - Shell Parameter Expansion](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
- [Linuxize - Extract a substring from a string in Bash](https://linuxize.com/post/bash-extract-substring/)