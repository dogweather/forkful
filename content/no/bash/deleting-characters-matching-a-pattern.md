---
title:                "Slette tegn som samsvarer med et mønster"
html_title:           "Arduino: Slette tegn som samsvarer med et mønster"
simple_title:         "Slette tegn som samsvarer med et mønster"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Slette karakterer som passer til et mønster er en måte å filtrere ut uønskede elementer i data på. Programmerere gjør det for å rense og manipulere input mer nøyaktig og effektivt.

## Hvordan:
For å slette karakterer som passer til et mønster i Bash, bruker vi `tr -d` kommando.

```Bash
echo "Hallo Verden 123" | tr -d '123'
```
Utdata vil være: 
```Bash
Hallo Verden 
```
Her har vi brukt `tr` med `-d` flag for å slette de gitte karakterene ('123') fra inputstrengen ("Hallo Verden 123").


## Dypdykk

Slette karakterer som passer til et mønster har røtter tilbake til tidlig Unix-tid. Det tilbyr en effektiv og rett på sak måte å håndtere tekst og data manipulering. 

Alternativet til `tr -d` er å bruke `sed`, en annen kraftig stream editor for filtrering og transformasjon av tekst.

```Bash
echo "Hallo Verden 123" | sed 's/[123]//g'
```
Dette gir samme resultat.

Hvordan `tr -d` fungerer bak kulissene avhenger av det spesifikke operativsystemet og Bash-versjonen. Generelt sett søker det gjennom inputstrengen, tegn for tegn, og ignorerer alle tegn som matcher det gitte mønsteret.

## Se Også
1. GNU `tr` man side: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
2. `sed` kommando i Linux: https://www.geeksforgeeks.org/sed-command-in-linux-unix-with-examples/
3. Bash Scripting Guide: https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_04_02.html