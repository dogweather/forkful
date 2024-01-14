---
title:                "Bash: Søk og erstatt tekst"
simple_title:         "Søk og erstatt tekst"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor 
Noen ganger når vi jobber med programmering eller skripting, må vi endre enkelte deler av teksten i filer for å få ønsket resultat. Dette kan være for å rette opp feil, oppdatere informasjon eller gjøre omfattende endringer. Ved å bruke søk og erstatt-funksjonen kan vi enkelt og effektivt håndtere disse oppgavene.

## Slik gjør du det 
For å søke og erstatte tekst i en fil i Bash, kan du bruke kommandoen `sed`. La oss si at vi ønsker å endre alle forekomster av "epler" til "appelsiner" i en fil kalt `frukt.txt`. Da vil kommandoen se slik ut:

```Bash
sed -i 's/epler/appelsiner/g' frukt.txt
```

La oss se på hver del av denne kommandoen i detalj. Først bruker vi flagget `-i` som står for "in-place", som betyr at endringene vil bli gjort direkte i filen. Dette sparer oss for å måtte lagre en midlertidig fil og deretter erstatte den originale filen. Deretter bruker vi `s` som står for "substitute", etterfulgt av `/epler/appelsiner/g`. Her forteller vi `sed` å søke etter forekomster av "epler" og erstatte det med "appelsiner", og `g` står for "global", som betyr at den vil erstatte alle forekomster av "epler" i filen. Til slutt gir vi navnet på filen vi ønsker å endre, `frukt.txt`.

La oss se et eksempel på hvordan filen `frukt.txt` kan se ut før og etter:

**Før:**

```
I dag skal jeg kjøpe epler til eplepaien min.
Epler er en av mine favoritt frukter.
```

**Etter:**

```
I dag skal jeg kjøpe appelsiner til eplepaien min.
Appelsiner er en av mine favoritt frukter.
```

Vi kan også bruke `sed` til å erstatte tekst i flere filer samtidig ved å bruke kommandoen `find` og pipe den til `sed`. La oss si at vi ønsker å gjøre den samme endringen i alle filer med navnet `mat.txt` i mappen vår. Da vil kommandoen se slik ut:

```Bash
find . -name mat.txt | xargs sed -i 's/epler/appelsiner/g'
```

Vi starter først med å bruke `find` sammen med flagget `-name` for å finne alle filene med navnet `mat.txt` i mappen vår (`.` betyr gjeldende mappe). Deretter bruker vi `xargs` for å pipe resultatet til `sed` og gjøre endringene i alle disse filene.

## Dykk dypere 
`sed` er bare ett av mange verktøy som kan brukes til å søke og erstatte tekst i Bash. Andre verktøy inkluderer `grep`, som kan brukes til å søke etter tekst i en fil, og `awk`, som kan brukes til å behandle og formatere tekst. Det kan være lurt å utforske disse verktøyene nærmere for å bli mer effektiv i håndteringen av tekstbehandling i Bash.

## Se også 
- [Linux Bash sed command](https://www.computerhope.com/unix/used.htm)
- [How to use sed command in linux/unix with examples](https://www.youtube.com/watch?v=GV0W7TeDseU)
- [Using sed to Find and Replace Text in Files](https://www.mkyong.com/linux/sed-command-how-to-use-with-example/)