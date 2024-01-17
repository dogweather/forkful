---
title:                "Skrive en tekstfil"
html_title:           "Bash: Skrive en tekstfil"
simple_title:         "Skrive en tekstfil"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive til en tekstfil i Bash er å legge inn tekst eller kommandoer i en fil som kan leses og utføres av datamaskinen din. Programmere gjør dette for å lagre og organisere informasjon som er nødvendig for å kjøre programmer eller skript.

## Slik gjør du:
Generatorer i Bash kan brukes til å skrive til en tekstfil ved å bruke ```echo``` kommandoen sammen med videresending av utgangen til en fil ved hjelp av ```>``` operatøren. For eksempel:

```
echo "Dette er en tekstfil" > filnavn.txt
```

Hvis filen allerede finnes, vil kommandoen overskrive innholdet. Hvis du vil legge til tekst i slutten av en eksisterende fil, kan du bruke ```>>``` operatøren i stedet:

```
echo "Mer tekst" >> filnavn.txt
```

For å bekrefte at teksten har blitt skrevet til filen, kan du bruke ```cat``` kommandoen:

```
cat filnavn.txt
```

Dette vil skrive ut innholdet i filen til terminalen.

## Dypdykk:
Å skrive til tekstfiler i Bash har vært en integrert del av programmering siden UNIX-systemet ble utviklet på 1970-tallet. Alternativene til å bruke ```echo``` kommandoen er å bruke andre Bash kommandoer som ```printf``` eller å bruke en tekstredigerer som ```vim``` eller ```nano```. Husk at hvis du bruker en tekstredigerer, må du lagre filen før du avslutter.

For å skrive til filer som krever tilgangsrettigheter, kan du bruke ```sudo``` kommandoen sammen med ```echo``` og ```>>``` for å unngå eventuelle tillatelsesproblemer.

## Se også:
- [Bash Manual](https://www.gnu.org/software/bash/manual/)
- [Bash Scripting Tutorial](https://ryanstutorials.net/bash-scripting-tutorial/)
- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/)