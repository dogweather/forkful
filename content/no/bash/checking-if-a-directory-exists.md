---
title:                "Bash: Sjekke om en mappe eksisterer"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Hvorfor
En vanlig oppgave for en programmerer er å sjekke om en bestemt mappe eksisterer. Dette er spesielt viktig når vi skriver skript som skal jobbe med filer og må navigere gjennom forskjellige mapper. Ved å lære hvordan man sjekker om en mappe eksisterer, kan vi sikre at skriptene våre fungerer riktig og unngå uventede feil.

# Hvordan gjøre det
For å sjekke om en mappe eksisterer i Bash, kan vi bruke kommandoen `test`. Denne kommandoen lar oss utføre forskjellige tester, inkludert å sjekke om en mappe eksisterer. Vi kan også bruke `if`-statment for å utføre handlinger basert på resultatet av testen.

```Bash
if test -d /sti/til/mappe; then
    # Handle når mappen eksisterer
else
    # Handle når mappen ikke eksisterer
fi
```

La oss se et enkelt eksempel på dette i handling. Vi har en mappe kalt "prosjekt" i hjemmemappen vår.

```Bash
if test -d ~/prosjekt; then
    echo "Mappen eksisterer"
else
    echo "Mappen eksisterer ikke"
fi
```

Når dette skriptet kjøres, vil det skrive ut "Mappen eksisterer" siden prosjekt-mappen faktisk eksisterer.

# Dypdykk
Vi kan også bruke `test`-kommandoen til å sjekke om en mappe ikke eksisterer ved å bruke utropstegn som foran testen.

```Bash
if test ! -d /sti/til/mappe; then
    # Handle når mappen ikke eksisterer
fi
```

Vi kan også bruke `if`-statment for å sjekke om to mapper har samme navn ved å bruke dobbel likhetstegn.

```Bash
mappe1="mappe"
mappe2="mappe"

if test "$mappe1" == "$mappe2"; then
    echo "Mappene har samme navn"
fi
```

Når vi prøver å kjøre disse to eksemplene, vil vi se at testen utføres riktig og den tilsvarende handlingen blir utført.

# Se også
- [BashGuide](https://mywiki.wooledge.org/BashGuide)
- [Bash Scripting Tutorial](https://ryanstutorials.net/bash-scripting-tutorial/)
- [Linuxcommand.org](https://linuxcommand.org/)