---
date: 2024-01-26 01:09:40.462112-07:00
description: "Hvordan: I Bash, har funksjoner v\xE6rt en m\xE5te \xE5 inndele kode\
  \ p\xE5 siden de tidlige versjonene. Historisk sett, har bruk av funksjoner v\xE6\
  rt i tr\xE5d med\u2026"
lastmod: '2024-04-05T22:50:54.982525-06:00'
model: gpt-4-1106-preview
summary: "I Bash, har funksjoner v\xE6rt en m\xE5te \xE5 inndele kode p\xE5 siden\
  \ de tidlige versjonene."
title: Organisering av kode i funksjoner
weight: 18
---

## Hvordan:
Lag en enkel funksjon i Bash:

```Bash
hilsen() {
  echo "Hei, $1!"
}
```

Bruk den ved å kalle på funksjonen med en parameter:

```Bash
hilsen "Verden"  # Utdata: Hei, Verden!
```

Funksjoner kan returnere verdier ved å bruke `return` for numeriske statuskoder (ikke for faktisk dataretur):

```Bash
legg_til() {
  return $(($1 + $2))
}

legg_til 3 4
echo $?  # Utdata: 7
```

Merk at `$?` fanger opp returverdien til den siste kommandoen, som er det numeriske resultatet av `legg_til`.

## Dypdykk
I Bash, har funksjoner vært en måte å inndele kode på siden de tidlige versjonene. Historisk sett, har bruk av funksjoner vært i tråd med prinsipper for strukturert programmering introdusert på 1960-tallet for å forbedre kodekvaliteten.

Alternativer til funksjoner inkluderer å source skriptfiler eller å bruke aliaser, men disse tilbyr ikke samme nivå av modularitet og gjenbruk.

En bemerkelsesverdig implementeringsdetalj i Bash er at funksjoner er førsteklasses borgere; de har ikke noe spesifikt deklarasjonsnøkkelord som `function` i andre språk, selv om `function` er valgfritt i Bash for lesbarhet. Funksjonens skop er også interessant – variabler er globale som standard med mindre de er erklært som lokale, noe som kan føre til uventet oppførsel hvis det ikke håndteres ordentlig.

## Se også
- Bash-manualen om Shell Functions: https://www.gnu.org/software/bash/manual/html_node/Shell-Functions.html
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/functions.html
- "Pro Bash Programming: Scripting the GNU/Linux Shell" for grundige funksjonsskripteringskonsepter og praksiser.
