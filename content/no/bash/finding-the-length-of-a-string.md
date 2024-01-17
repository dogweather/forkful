---
title:                "Å finne lengden på en streng"
html_title:           "Bash: Å finne lengden på en streng"
simple_title:         "Å finne lengden på en streng"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Hva og hvorfor?
Å finne lengden på en streng er en vanlig oppgave for programmerere. Det innebærer å telle antall tegn i en tekst eller en variabel. Dette kan være nyttig for å validere brukerinput, lage begrensninger for tekstinput eller å formatere utdata på en bestemt måte.

# Hvordan:
For å finne lengden på en streng i Bash, kan vi bruke kommandoen "echo" sammen med parameteren "-n" for å ikke legge til et nytt linjeskift i utdataen. Vi kan deretter bruke "wc" (word count) kommandoen med parameteren "-c" for å telle antall tegn i strengen. Her er et eksempel:

```Bash
echo -n "Hei, verden!" | wc -c
```
Output: 12

Vi kan også bruke "expr" kommandoen sammen med "length" funksjonen for å finne lengden på en variabel. Her er et eksempel på dette:

```Bash
hello="Hei, verden!"
echo "Lengden på strengen hello er $(expr length $hello)"
```
Output: Lengden på strengen hello er 12

# Dypdykk:
Å finne lengden på en streng er en vanlig oppgave som går langt tilbake i programmeringshistorien. I eldre programmeringsspråk, som for eksempel C, må programmet selv håndtere å telle lengden på en streng. I moderne språk som Bash, har vi mer effektive kommandoer for å gjøre dette. Alternativet til å bruke "wc" kommandoen, er å bruke "expr" kommandoen med "length" funksjonen, som vi så i eksempelet.

# Se også:
- [Unix lengde kommandodokumentasjon](https://linux.die.net/man/1/length)
- [Bash man-side for "expr" kommandoen](https://linux.die.net/man/1/expr)