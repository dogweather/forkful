---
title:                "Bash: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang ønsket å vite om en mappe eksisterer i Bash-programmet ditt? Kanskje du ønsker å utføre en bestemt handling bare hvis mappen allerede finnes, eller kanskje du vil lage en ny mappe hvis den ikke eksisterer ennå. Uansett årsak, å ha evnen til å sjekke om en mappe eksisterer i Bash kan spare deg for mye frustrasjon og feilsøking. I denne blogginnlegget skal vi se på hvordan du enkelt kan sjekke om en mappe eksisterer i Bash-programmet ditt.

## Hvordan gjøre det

For å sjekke om en mappe eksisterer i Bash, kan du bruke kommandoen `test` eller `[[ ]]`. Dette fungerer for både vanlige Bash-skript og interaktive terminaler. La oss se på et eksempel:

```
if test -d "min-mappe"
then
  echo "Mappe eksisterer allerede"
else
  echo "Mappe må opprettes"
fi
```

Her sjekker vi om mappen "min-mappe" eksisterer ved å bruke `-d` flagget i `test` -kommandoen. Hvis mappen allerede finnes, vil skriptet skrive ut "Mappe eksisterer allerede". Hvis ikke, vil det skrive ut "Mappe må opprettes". Du kan også bruke `!` foran `-d` for å sjekke om mappen ikke eksisterer.

En annen måte å gjøre dette på er å bruke `[[ ]]` syntaksen:

```
if [[ -d "min-mappe" ]]
then
  echo "Mappe eksisterer allerede"
else
  echo "Mappe må opprettes"
fi
```

Dette fungerer på samme måte som `test` -kommandoen, men med `[[ ]]` syntaksen kan du også bruke den logiske operatoren `&&` for å sjekke om flere mapper eksisterer.

## Dypdykk

Nå som du vet hvordan du sjekker om en mappe eksisterer i Bash, kan det være nyttig å vite hvordan du kan unngå å få en feilmelding hvis mappen ikke eksisterer. En vanlig feil som kan oppstå er at Bash vil skrive ut en feilmelding hvis du prøver å liste innholdet i en mappe som ikke eksisterer. For å unngå dette, kan du bruke `||` operatoren hvis mappen ikke eksisterer:

```
ls "min-mappe" || echo "Mappen eksisterer ikke"
```

Her vil skriptet prøve å liste innholdet i "min-mappe", men hvis den ikke eksisterer, vil det skrive ut "Mappen eksisterer ikke" istedenfor å gi en feilmelding.

Du kan også bruke `mkdir` kommandoen sammen med `-p` flagget for å automatisk lage mappen hvis den ikke allerede eksisterer:

```
mkdir -p "min-mappe"
```

Dette fungerer også hvis du vil opprette en undermappe i en eksisterende mappe.

## Se også

For mer informasjon og flere eksempler, kan du se disse ressursene:

- [Bash referansedokumentasjon](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html)
- [Bash sjekk om filen / mappen eksisterer](https://linuxize.com/post/bash-check-if-file-exists/)
- [Bash dokumentasjon på `test` -kommandoen](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html#Bash-Conditional-Expressions)