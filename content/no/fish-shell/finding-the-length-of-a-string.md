---
title:                "Fish Shell: Å finne lengden av en streng"
simple_title:         "Å finne lengden av en streng"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å finne lengden på en streng er en vanlig oppgave i programmering som kan være nyttig for å håndtere og manipulere tekst. Ved å bruke Fish Shell, kan du enkelt finne lengden på en streng ved hjelp av noen få kommandoer.

## Hvordan

For å finne lengden på en streng i Fish Shell, kan du bruke ```string len```kommandoen. Her er et enkelt eksempel:

```
Fish Shell> string len "Hei, verden!"
12
```

Som du kan se, returnerer ```string len```kommandoen antallet tegn i den gitte strengen.

Du kan også finne lengden på en variabel ved å bruke ```string len```kommandoen med variabelnavnet som argument:

```
Fish Shell> set tekst "Hello, world!"
Fish Shell> string len $text
13
```

Hvis variabelen inneholder en liste av strenger, vil ```string len```kommandoen returnere antallet strenger i listen:

```
Fish Shell> set liste "Eple Jordbær Drue"
Fish Shell> string len $liste
3
```

## Dykk dypere

Det er viktig å merke seg at ```string len```kommandoen regner med mellomrom og andre tegn i lengden på en streng. Dette betyr at følgende eksempel vil returnere 14 i stedet for 12:

```
Fish Shell> string len "Hei,  verden!"
14
```

Det er også verdt å nevne at du kan bruke en kombinasjon av kommandoer for å finne lengden på en streng. For eksempel, hvis du skal finne lengden på en streng og legge til en annen verdi, kan du bruke følgende format:

```
Fish Shell> set tekst "Hello, world!"
Fish Shell> echo (expr string len $tekst + 2)
15
```

Dette kan være nyttig for å manipulere tekst i lengden av en streng. Det gir deg også muligheten til å kombinere flere kommandoer for å oppnå ønsket resultat.

## Se også

- Fish Shell dokumentasjon for "string len": https://fishshell.com/docs/current/cmds/string-len.html
- Sammenligning av strenglengde i forskjellige programmeringsspråk: https://www.rosettacode.org/wiki/String_length