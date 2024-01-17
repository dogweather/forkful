---
title:                "Sjekke om en mappe eksisterer"
html_title:           "Bash: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sjekke om en mappe eksisterer er en kodepraksis som brukes av Bash-programmerere for å sikre at en bestemt mappe finnes før man fortsetter å kjøre videre kode.

## Hvordan:
For å sjekke om en mappe eksisterer kan du bruke kommandolinjeverktøyet `test` med flagget `-d`, som står for "directory". Dette vil teste om mappen eksisterer og returnere en sannhetsverdi (true/false). Se eksempler nedenfor. 

```Bash
mkdir testmappe
if test -d testmappe; then
  echo "testmappe eksisterer"
else
  echo "testmappe eksisterer ikke"
fi
```

```Bash
if test -d ikkeeksisterendemappe; then
  echo "ikkeeksisterendemappe eksisterer"
else
  echo "ikkeeksisterendemappe eksisterer ikke"
fi
```

Dette kan også gjøres ved å bruke nøkkelordet `[[` og `]]` for å indikere en betingelse i Bash. Det ville sett slik ut: 

```Bash
mkdir testmappe
if [[ -d testmappe ]]; then
  echo "testmappe eksisterer"
else
  echo "testmappe eksisterer ikke"
fi
```

## Dypdykk:
Sjekke om en mappe eksisterer har vært en del av Unix- og Linux-systemer siden starten. Det finnes også andre måter å sjekke om en mappe eksisterer på, for eksempel ved bruk av kommandoen `stat`, men `test -d` fungerer godt for de fleste tilfeller. Implementeringen av denne kommandoen kan variere mellom forskjellige operativsystemer, men konseptet er det samme.

## Se også:
- `man test`: Man-siden for `test`-kommandoen.
- [Sjekk om en fil eller mappe eksisterer i Bash](https://www.baeldung.com/linux/bash-check-if-file-directory-exists): En tutorial med flere eksempler for å sjekke om filer og mapper eksisterer i Bash.
- [Kommandoen `stat`](https://www.baeldung.com/linux/bash-stat-command): En annen kommando for å få informasjon om en fil eller mappe i Bash.