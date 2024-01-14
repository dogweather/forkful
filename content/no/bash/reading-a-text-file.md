---
title:                "Bash: Lesing av en tekstfil"
simple_title:         "Lesing av en tekstfil"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Når man jobber med Bash-programmering, vil man ofte støte på situasjoner der man må lese informasjon fra en tekstfil. Dette kan være for å hente inn data, behandle informasjon eller for å automatisk generere rapporter. Å kunne lese en tekstfil er derfor en viktig ferdighet å ha i verktøykassen når man driver med Bash-programmering.

## Hvordan

Å lese en tekstfil i Bash er enkelt og kan gjøres ved å bruke kommandoen "cat". Denne kommandoen viser innholdet i en tekstfil direkte i terminalen. For å lese innholdet og lagre det i en variabel kan man bruke kommandoen "read" og peke til tekstfilen. For eksempel:

```Bash
tekstfil="mitttekstfil.txt"
while read line
do
  # gjør noe med hver linje i tekstfilen
  echo $line # for eksempel skriv ut linjen i terminalen
done < "$tekstfil"
```

Dette vil lese hver linje i tekstfilen og gjøre en handling med den. Man kan også lese innholdet i en tekstfil på en mer spesifikk måte ved å bruke grep-kommandoen. For eksempel hvis man kun vil lese linjer som starter med "Navn:", kan man bruke dette uttrykket:

```Bash
grep "Navn:" mitttekstfil.txt
```

Dette er et enkelt eksempel, men med grep-kommandoen kan man utnytte regulære uttrykk for å lese mer spesifikt fra en tekstfil.

## Dypdykk

Når man leser en tekstfil i Bash, kan det være nyttig å vite at det finnes forskjellige metoder for å lese innholdet. I tillegg til "cat" og "read" kan man også bruke kommandoen "awk" og "sed" for å lese og manipulere innholdet i en tekstfil. Disse kommandoene gir mer avanserte muligheter for å filtrere og behandle data fra en tekstfil. Det kan være lurt å lese mer om disse kommandoene og å øve seg på dem for å bli en mer effektiv Bash-programmerer.

## Se også

- [Bash-programmering](https://en.wikipedia.org/wiki/Bash_%28Unix_shell%29)
- [Lesing av filer i Bash](https://linuxize.com/post/bash-read-file/)
- [Bash-grep kommandoen](https://www.geeksforgeeks.org/grep-command-in-linux-unix-with-examples/)