---
title:                "Fish Shell: Opprettelse av en midlertidig fil"
simple_title:         "Opprettelse av en midlertidig fil"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor
Opprettelse av midlertidige filer er en viktig del av programmering når du trenger å lagre midlertidige data som skal brukes i korte perioder. Det er spesielt nyttig når du jobber med batchbehandling eller midlertidige filer mens du kjører andre programmer.

## Slik gjør du det
Først må du åpne Fish Shell og navigere til mappen du vil lagre den midlertidige filen i. Deretter kan du opprette en fil ved hjelp av følgende kommando:

```Fish Shell
mktemp FILENAME
```
Du kan erstatte “FILENAME” med navnet du vil gi den midlertidige filen din. Dette vil opprette en tom fil med det gitte navnet.

For å legge til innhold i filen, kan du bruke en av flere metoder. En måte er å bruke “echo” kommandoen for å legge til tekst i filen:

```Fish Shell
echo "Dette er innholdet i filen" > FILENAME
```

En annen metode er å bruke “cat” kommandoen for å skrive innholdet fra en annen fil til den midlertidige filen:

```Fish Shell
cat EXAMPLE_FILE > FILENAME
```

Når du er ferdig med å bruke den midlertidige filen, kan du slette den ved hjelp av følgende kommando:

```Fish Shell
rm FILENAME
```

## Dypdykk
En interessant funksjon ved midlertidige filer er at de automatisk får et unikt og tilfeldig navn hver gang de opprettes. Dette er nyttig for å unngå konflikter med andre filer som allerede finnes i mappen.

Det er også mulig å opprette en midlertidig mappe i stedet for en fil ved å bruke “mktemp -d” kommandoen.

## Se også
- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/index.html)
- [Linux User and Developer – Creating Temporary Files and Directories](https://www.linuxuser.co.uk/tutorials/creating-temporary-files-and-directories)
- [Code Snipcademy – Temporary Files in Python](https://code.snipcademy.com/tutorials/python-programming/temporary-files)