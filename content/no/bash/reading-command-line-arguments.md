---
title:                "Lesing av kommandolinjeargumenter"
html_title:           "Bash: Lesing av kommandolinjeargumenter"
simple_title:         "Lesing av kommandolinjeargumenter"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?
Readingkommando linjeargumenterer en viktig del av å skrive Bashprogrammer. Det lar programmererne lese input fra terminalen og bruke det til å tilpasse sin kode. Dette gir større fleksibilitet og muligheten til å lage mer dynamiske programmer.

## Hvordan gjøre det:
Bruk kommandoen "$1" for å lese den første kommandolinjeparameteren, "$2" for den andre og så videre. Hvis du for eksempel vil få programmet ditt til å si "Hei, [navn]", kan du skrive følgende:

```Bash
echo "Hei, $1"
```

Når du kjører programmet med kommandolinjen "bash mittprogram.sh Ola", vil det skrive ut "Hei, Ola".

## Dypdykk:
Å lese kommandolinjeargumenter har vært en del av Bash siden starten. Det gir lignende funksjonalitet som i andre programmeringsspråk som Python, Perl og Ruby. Noen programmer kan også bruke miljøvariabler til å lese input fra brukeren.

## Se også:
Her er noen nyttige ressurser for å lære mer om å lese kommandolinjeargumenter i Bash:

- Offisiell dokumentasjon: https://www.gnu.org/software/bash/manual/html_node/Shell-Parameters.html
- En tutorial med eksempler: https://www.tutorialspoint.com/unix/unix-using-arguments.htm
- En dypere forståelse av kommandolinjebehandling: https://www.tldp.org/LDP/abs/html/intandnonint.html#COMMANDLINEREF