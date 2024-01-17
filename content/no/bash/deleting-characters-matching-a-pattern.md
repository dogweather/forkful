---
title:                "Sletting av tegn som sammenfaller med et mønster"
html_title:           "Bash: Sletting av tegn som sammenfaller med et mønster"
simple_title:         "Sletting av tegn som sammenfaller med et mønster"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sletting av tegn som matcher et mønster, er en vanlig praksis blant programmører for å manipulere tekstfiler og data. Dette gjøres vanligvis for å filtrere ut uønsket eller unødvendig informasjon.

## Slik gjør du:
Her er to eksempler på hvordan du kan slette tegn som matcher et mønster ved hjelp av Bash-programmering:

```Bash
# Eksempel 1: Slette alle tall fra en tekstfil
sed 's/[0-9]//g' filnavn.txt

# Eksempel 2: Slette alle vokaler fra en setning
grep -o '[^aeiou]' setning.txt
```

For eksempel, hvis vi kjører eksempel 1 på en tekstfil som inneholder "Hei 123, dette er en test", vil outputen bli "Hei , dette er en test". Og ved å kjøre eksempel 2 på setningen "Dette er en test", vil outputen bli "Dtt r n tst".

## Dypdykk:
Sletting av tegn som matcher et mønster ble først introdusert i Unix-programmeringsspråket. I tillegg til Bash, er det også mulig å gjøre dette ved hjelp av andre programmeringsspråk som Perl og Python. Fordelen med å bruke Bash er at det er et kraftig og fleksibelt språk som er installert på de fleste operativsystemer.

## Se også:
- [Bash Manual](https://www.gnu.org/software/bash/manual/bash.html)
- [Regex Tutorial](https://www.regular-expressions.info/tutorial.html)
- [Bash scripting for beginners](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)