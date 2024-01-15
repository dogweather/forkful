---
title:                "Lage en midlertidig fil"
html_title:           "Fish Shell: Lage en midlertidig fil"
simple_title:         "Lage en midlertidig fil"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange ganger når vi skriver script eller automatiserer prosesser, trenger vi å lagre midlertidige data. Dette kan være for å lagre output fra et program eller for å bruke som midlertidig lagring mens scriptet kjører. I Fish Shell er det en enkel kommando for å opprette en midlertidig fil som kan brukes til dette formålet.

## Slik gjør du det

Du kan bruke følgende kommando for å opprette en midlertidig fil i Fish Shell:

```Fish Shell
mktemp
```

Dette vil opprette en midlertidig fil med et tilfeldig navn i det midlertidige mappen på systemet ditt. Du kan også spesifisere et prefiks for navnet på filen ved å legge til `-p` opsjonen, for eksempel:

```Fish Shell
mktemp -p prefix_
```

Du kan også velge å opprette en midlertidig mappe i stedet for en fil ved å legge til `-d` opsjonen, for eksempel:

```Fish Shell
mktemp -d
```

I tillegg kan du bruke opsjonen `-t` for å spesifisere en annen katalog enn standard midlertidig mappe for å opprette filen i. Og hvis du vil at filen skal slettes automatisk når den ikke lenger er i bruk, kan du bruke `-u` opsjonen.

## Dykk dypere

Når du oppretter en midlertidig fil ved hjelp av `mktemp`, vil den automatisk brukes til å lagre output fra kommandoer. Dette gjør det enklere å behandle og manipulere denne informasjonen videre i et script. Du kan også bruke kommandoen `rm` for å slette filen når den ikke lenger er nødvendig, for eksempel:

```Fish Shell
mktemp | rm
```

Det er også verdt å merke seg at du kan opprette flere midlertidige filer ved å bruke `mktemp` flere ganger i samme script. Dette kan være nyttig hvis du trenger å lagre output fra flere kommandoer til separate filer.

## Se også

For mer informasjon om `mktemp` kommandoen og hvordan du kan bruke den, kan du sjekke ut følgende ressurser:

- [The Fish Shell User Manual](https://fishshell.com/docs/current/index.html)
- [mktemp command man page](https://linux.die.net/man/1/mktemp)
- [How to Use Temporary Files in Bash Scripts](https://linuxize.com/post/bash-temporary-file/)