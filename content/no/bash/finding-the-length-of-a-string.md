---
title:                "Bash: Finne lengden til en streng"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Hvorfor
Å finne lengden på en streng er en essensiell oppgave i Bash-programmering. Det lar oss beregne og manipulere data på en effektiv måte. Denne blogginnlegget vil vise deg hvordan du kan gjøre det på en enkel måte.

## Hvordan
Du kan finne lengden på en streng ved å bruke `echo`, `wc` og `cut` kommandoene i Bash. Her er et eksempel på en kode som finner lengden på en streng og lagrer den i en variabel:

```Bash
string="Dette er en teststreng"
len=$(echo -n "$string" | wc -c | cut -d' ' -f1)
echo "Lengden på strengen er $len"
```

Output:
```
Lengden på strengen er 20
```

La oss se på hva som skjer her. Først definerer vi en variabel `string` som inneholder en teststreng. Deretter bruker vi `echo` kommandoen sammen med `-n` flagget for å utelate linjeskift og `wc` kommandoen for å telle antall tegn i strengen. Til slutt bruker vi `cut` kommandoen for å få tak i det første feltet som er lengden på strengen, og lagrer denne verdien i en variabel `len`. Til slutt, bruker vi `echo` kommandoen igjen for å skrive ut lengden på strengen.

Dette er bare én måte å finne lengden på en streng på, det finnes mange andre metoder avhengig av hva slags oppgave du jobber med. Her er et annet eksempel på hvordan du kan bruke `awk` kommandoen for å finne lengden på en streng:

```Bash
len=$(echo "$string" | awk '{print length}')
```

## Dypdykk
Hvis du vil dykke dypere inn i dette emnet, er det viktig å forstå hva som skjer "under panseret" når du bruker kommandoene vi har nevnt. Når du bruker `echo` kommandoen med `-n` flagget, forteller du den å ikke legge til et linjeskift på slutten av utputen. Dette er viktig fordi `wc` kommandoen teller linjeskift som tegn. `wc` kommandoen står for "word count" og den returnerer antall linjer, ord og tegn i en fil eller standard input. Ved å bruke `cut` kommandoen med `-d` flagget kan du spesifisere hvilket tegn som skal brukes som deleparameter, i vårt tilfelle er dette et mellomrom (som er standardverdien). Når du angir `-f` flagget kan du spesifisere hvilket felt du ønsker å få tak i, ettersom `wc` kommandoen returnerer flere felt.

## Se også
* [Bash Cheatsheet in Norwegian](https://gist.github.com/esphen/4985bcabe5a2dbd4b582)
* [Bash scripting for beginners](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
* [Bash Scripting Tutorial - Explain Shell](https://explainshell.com/explain?cmd=echo+-n+%22hello+world%22+%7C+wc+-c+%7C+cut+-d%27+%27+-f1%0D%0A)