---
title:                "Ekstrahering av delstrenger"
html_title:           "Bash: Ekstrahering av delstrenger"
simple_title:         "Ekstrahering av delstrenger"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Et vanlig problem for programmerere er å måtte håndtere strenger med uønsket data. Extracting substrings er en måte å løse dette på, ved å hente ut en del av en streng som oppfyller bestemte betingelser. Dette kan gjøres for å forenkle håndtering av data eller for å foreta spesifikke operasjoner på bestemte deler av en streng.

Programmerere bruker extracting substrings fordi det gjør det enklere å manipulere data, spesielt når man jobber med store datamengder. Det kan også være nødvendig for å oppfylle spesifikke krav til dataformat.

## Hvordan:
Eksempelkoden nedenfor viser hvordan du kan bruke ```Bash``` for å extracte substrings fra en streng:

```
string="Hei dette er en test"
substring=${string:4:6}

echo "$substring" # Vil skrive ut "dette er"
```

Her brukes ```substring``` for å extracte 6 tegn fra posisjon 4 i ```string```. Dette gir resultatet "dette er". Man kan også bruke variabler for å angi startposisjon og lengden på substringen, som vist i eksempelet under:

```
start=4
length=6
substring=${string:start:length}

echo "$substring" # Vil skrive ut "dette er"
```

En annen nyttig måte å extracte substrings på er å bruke ```grep``` kommandoen sammen med regulære uttrykk. Dette gjør det mulig å extracte deler av strenger basert på et mønster, for eksempel etter bestemte ord eller tall.

## Dypdykk:
Historisk sett har extracting substrings vært brukt siden de første programmerings språkene, som Fortran og Basic. I dag er det fortsatt en viktig funksjon i mange programmeringsspråk, inkludert Bash.

Alternativer til extracting substrings inkluderer å bruke metoder som ```substring``` i noen programmeringsspråk, eller å bruke regex for mer avansert matching. Implementeringen av extracting substrings varierer også avhengig av programmeringsspråk og operativsystem.

## Se også:
- [The Bash Guide](https://guide.bash.academy/) - en omfattende guide til Bash programmering for nybegynnere og erfarne brukere.
- [Regular Expressions in Bash](https://www.gnu.org/software/grep/manual/html_node/Regular-Expressions.html) - mer informasjon om regulære uttrykk og hvordan de kan brukes til å extracte substrings.
- [Mye å lære Bash](https://dev.to/awwsmm/learn-enough-bash-to-be-dangerous) - en artikkel med tips og triks for å bli bedre på Bash programmering.