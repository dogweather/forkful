---
title:    "Bash: Sjekke om en mappe finnes"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sjekke om en mappe eksisterer er en viktig del av Bash-programmering. Dette kan hjelpe deg med å sikre at skriptene dine kjører som de skal og unngå uventede feil.

## Hvordan

For å sjekke om en mappe eksisterer i Bash, kan du bruke `test` kommandoen med flagget `-d` etterfulgt av mappenavnet.

```Bash
if test -d mappenavn
then
    echo "Mappen eksisterer!"
else
    echo "Mappen eksisterer ikke."
fi
```

Dette skriptet vil enten gi ut "Mappen eksisterer!" hvis mappen finnes, eller "Mappen eksisterer ikke." hvis mappen ikke finnes.

En annen måte å sjekke på er å bruke `[[`brackets`]]` med flagget `-d` etterfulgt av mappenavnet.

```Bash
if [[ -d mappenavn ]]
then
    echo "Mappen eksisterer!"
else
    echo "Mappen eksisterer ikke."
fi
```

Denne metoden vil også gi samme resultat som det første skriptet.

## Dypdykk

I Bash kan du også bruke `test` kommandoen med flagget `-e` for å sjekke om en fil eller mappe eksisterer. Dette vil sjekke både filer og mapper, mens `test -d` kun sjekker om en mappe eksisterer.

En annen ting å merke seg er at både `test` og `[[`brackets`]]` trenger absolutte stier for å sjekke om en mappen eksisterer. Hvis du ønsker å bruke relativ sti, kan du bruke variabelen `$PWD` (present working directory) etterfulgt av mappenavnet.

```Bash
if [[ -d $PWD/mappenavn ]]
then
    echo "Mappen eksisterer!"
else
    echo "Mappen eksisterer ikke."
fi
```

## Se Også

- [Bash-skripting for nybegynnere](https://www.finn bruker tips.com/bash-scripting-for-nybegynnere)
- [Bash dokumentasjon for `test` og `[[`brackets`]]`](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html)
- [5 vanlige Bash-programmeringsfeil og hvordan du kan unngå dem](https://www.examples.com/mistakes-to-avoid-when-bash-programming/)