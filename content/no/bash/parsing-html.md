---
title:                "Ikke noe Kommentar her!"
html_title:           "Bash: Ikke noe Kommentar her!"
simple_title:         "Ikke noe Kommentar her!"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/parsing-html.md"
---

{{< edit_this_page >}}

## Hvorfor
Det er forskjellige situasjoner der du kan støte på behov for å parse HTML i Bash-programmering, som for eksempel å lese eller analysere innhold på et nettsted, automatisk hente data fra en nettside, eller generere rapporter basert på HTML-maler.

## Hvordan
Det første trinnet for å parse HTML i Bash er å laste ned og installere et passende verktøy for dette formålet. Et populært alternativ er "pup" som kan installeres ved hjelp av følgende kommando:
```Bash
brew install pup
```

For å starte parsingen, må du først laste ned innholdet på nettsiden ved hjelp av "curl" kommandoen og deretter bruke "pup" for å filtrere ut ønsket informasjon fra HTML-koden. For eksempel, hvis du ønsker å finne alle overskriftene på en nettside, kan du bruke følgende kommando:
```Bash
curl -s https://www.example.com | pup 'h1' | tr -d '\n' && echo
```

Dette vil skrive ut alle overskriftene på nettsiden du har hentet og vil også formatere det i et leselig format uten linjeskift.

## Dypdykk
Selv om "pup" er et godt verktøy, kan det være begrenset når det kommer til komplekse HTML-strukturer. I slike tilfeller kan du bruke "xmlstarlet" som tilbyr et bredere utvalg av kommandoer for å håndtere XML og HTML-data.

En annen mulighet er å bruke "awk" kommandoen til å parse HTML. Dette er spesielt nyttig hvis du jobber med store datasett og trenger å filtrere ut spesifikke deler av HTML-koden basert på et mønster.

## Se også
+ Les mer om pup: https://github.com/ericchiang/pup
+ Utforsk mulighetene med xmlstarlet: https://xmlstar.sourceforge.io/
+ Lær mer om awk: https://www.gnu.org/software/gawk/manual/gawk.html