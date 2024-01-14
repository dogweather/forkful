---
title:    "Bash: Sammenslåing av strenger"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kombinere strenger i Bash-programmering kan være nyttig for å lage dynamiske og fleksible skript som kan håndtere variabelt innhold. Det kan også være en effektiv måte å formatere utdata på.

## Hvordan

For å kombinere strenger i Bash, bruker du konkatineringsoperatøren "=". Dette vil sette sammen to strenger side om side og danne en lengre streng.

Et eksempel på dette kan være å kombinere to strenger som inneholder navn og alder. Vi begynner med å definere variablene:

```Bash
navn="Marius"
alder="25 år gammel"
```

Deretter bruker vi konkateneringsoperatøren for å sette sammen de to strengene og lagre dem i en ny variabel:

```Bash
info="$navn er $alder."
echo $info
```

Output vil være:

```
Marius er 25 år gammel.
```

Det er også mulig å kombinere flere strenger sammen ved å bruke flere konkateneringsoperatører, som vist i dette eksemplet:

```Bash
fornavn="Eline"
etternavn="Lund"
epost="@example.com"
```

Vi kan da kombinere disse for å lage en e-postadresse ved å bruke flere konkateneringsoperatører:

```Bash
epostadresse="$fornavn$etternavn$epost"
echo $epostadresse
```

Output vil være:

```
ElineLund@example.com
```

## Dykk dypere

I Bash er det flere måter å kombinere strenger på, som for eksempel ved bruk av "printf" kommandoen eller ved å bruke "here" dokumenter. Det er også viktig å være oppmerksom på at det finnes forskjellige måter å håndtere spesialtegn og mellomrom mellom strenger på.

For å lære mer om å kombinere strenger i Bash, kan du utforske disse ressursene:

- [The Bash man page](https://linux.die.net/man/1/bash)
- [Bash Guide for Nybegynnere](https://www.pa.msu.edu/~abtajora/BashProgABS-guide.pdf)
- [Bash Scripting Tutorial](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [Bash Concatenation - Manipulating Strings in Bash](https://jimshaver.net/2019/02/22/bash-concatenation-manipulating-strings-in-bash/)

## Se også

- [Bash Arrays - Working with Arrays in Bash](https://github.com/mariusbrataas/bash-arrays)
- [Bash IF...ELSE - How to Use Conditional Statements in Bash](https://github.com/mariusbrataas/bash-if-else)
- [Bash Loops - How to Use For, While, and Until Loops in Bash](https://github.com/mariusbrataas/bash-loops)