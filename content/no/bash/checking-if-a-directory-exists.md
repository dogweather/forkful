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

## Why
Å sjekke om en mappe eksisterer kan være nyttig når du arbeider med Bash-scripts og trenger å utføre forskjellige handlinger basert på om en mappe allerede finnes eller ikke.

## How To
For å sjekke om en mappe eksisterer i Bash, kan du bruke kommandoen `test` sammen med parameteren `-d`. Dette vil returnere en sann verdi hvis mappen eksisterer og en falsk verdi hvis den ikke gjør det. Her er et eksempel på hvordan dette kan implementeres i et Bash-script:

```Bash
if test -d <mappe_navn>; then
    echo "<mappe_navn> eksisterer"
else
    echo "<mappe_navn> eksisterer ikke"
fi
```

Du kan også bruke konditionelle uttrykk sammen med `test` for å utføre forskjellige handlinger basert på om mappen eksisterer eller ikke. Her er et eksempel på et kondisjonelt uttrykk som sjekker om en mappe eksisterer og deretter oppretter mappen hvis den ikke finnes:

```Bash
if ! test -d <mappe_navn>; then
    mkdir <mappe_navn>
fi
```

## Deep Dive
I Bash representerer `-d`-parameteren for `test`-kommandoen et logisk uttrykk for å sjekke om en fil finnes og om den er en mappe. Dette kan også gjøres ved å bruke `ls`-kommandoen og pipelining det til `grep` for å sjekke om mappenavn matcher det gitte navnet. Det er også mulig å bruke variabler i `test`-kommandoen for å sjekke om en variabel som inneholder en mappebane faktisk refererer til en eksisterende mappe.

## Se også
- [Bash Manual](https://www.gnu.org/software/bash/manual/bash.html)
- [test kommandoen](https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#Bash-Builtins)
- [ls kommandoen](https://www.gnu.org/software/bash/manual/html_node/Shell-Builtin-Commands.html#Shell-Builtin-Commands)