---
title:                "Sjekker om en mappe eksisterer"
aliases:
- /no/bash/checking-if-a-directory-exists/
date:                  2024-02-03T19:06:50.514844-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sjekker om en mappe eksisterer"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva og hvorfor?

I Bash-programmering er det å sjekke om en mappe eksisterer en essensiell kontrollmekanisme som brukes for å verifisere tilstedeværelsen av en mappe før man utfører filoperasjoner. Denne sjekken er avgjørende for å unngå feil som å forsøke å få tilgang til eller endre mapper som ikke eksisterer, og sikrer en jevnere og mer forutsigbar skriptkjøring.

## Hvordan:

I sin kjerne tillater Bash deg å sjekke for eksistensen av en mappe ved å bruke betingede uttrykk og `-d`-operatøren. Nedenfor er et enkelt eksempel som demonstrerer hvordan du utfører denne sjekken.

```bash
if [ -d "/sti/til/mappe" ]; then
    echo "Mappen eksisterer."
else
    echo "Mappen eksisterer ikke."
fi
```

Eksempel på utskrift (hvis mappen eksisterer):
```
Mappen eksisterer.
```

Eksempel på utskrift (hvis mappen ikke eksisterer):
```
Mappen eksisterer ikke.
```

For mer komplekse skripter er det vanlig å kombinere sjekken med andre operasjoner, som å opprette mappen hvis den ikke eksisterer:

```bash
DIR="/sti/til/mappe"
if [ -d "$DIR" ]; then
    echo "$DIR eksisterer."
else
    echo "$DIR eksisterer ikke. Oppretter nå..."
    mkdir -p "$DIR"
    echo "$DIR opprettet."
fi
```

Eksempel på utskrift (hvis mappen ikke eksisterer og deretter blir opprettet):
```
/sti/til/mappe eksisterer ikke. Oppretter nå...
/sti/til/mappe opprettet.
```

Selv om Bash i seg selv tilbyr robuste verktøy for slike sjekker, finnes det ingen populære tredjepartsbiblioteker spesifikt for denne oppgaven, ettersom de innfødte Bash-kommandoene er fullt ut kapable og effektive for validering av mappetilstedeværelse.
