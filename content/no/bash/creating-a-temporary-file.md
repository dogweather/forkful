---
title:                "Oppretting av en midlertidig fil"
html_title:           "Bash: Oppretting av en midlertidig fil"
simple_title:         "Oppretting av en midlertidig fil"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Opprette en midlertidig fil betyr å lage en midlertidig fil som kun skal brukes for en begrenset periode. Dette er en vanlig praksis blant programmører for å lagre data midlertidig mens et program utfører en oppgave.

## Slik gjør du:
For å opprette en midlertidig fil i Bash, kan du bruke kommandoen ```mktemp``` etterfulgt av filnavnet du ønsker å bruke. For eksempel: 

```
Bash mktemp temp.txt
```

Dette vil opprette en midlertidig fil med navnet "temp.txt" i din nåværende mappe. Du kan også bruke flags som ```-d``` for å opprette en midlertidig mappe og ```-p``` for å spesifisere en annen mappe hvor den midlertidige filen skal opprettes.

## Dykk ned:
Opprettelse av midlertidige filer har vært en vanlig praksis i mange år, spesielt før datastøtte og ressurser var like tilgjengelige som i dag. Alternativene til midlertidige filer inkluderer å lagre data i minnet, noe som kan være risikabelt da data kan gå tapt hvis programmet får en feil, eller å bruke permanente filer, noe som kan føre til overbelastning av systemet med unødvendige filer.

For å implementere oppretting av midlertidige filer må Bash først sjekke om det finnes en eksisterende fil med samme navn, og hvis den gjør det, vil den generere et nytt navn for den midlertidige filen før den opprettes. Bash vil også sørge for at den midlertidige filen blir slettet når programmet avsluttes.

## Se også:
For mer informasjon om å opprette midlertidige filer i Bash, kan du se på dokumentasjonen for ```mktemp``` kommandoen. Du kan også lese mer om hvordan å bruke midlertidige filer på Linux-tips nettsiden eller i Bash-manualen.