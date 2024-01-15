---
title:                "Leser en tekstfil"
html_title:           "Fish Shell: Leser en tekstfil"
simple_title:         "Leser en tekstfil"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Lurer du på hvordan du kan lese tekstfiler ved hjelp av Fish Shell? Da er du på rett sted! I denne artikkelen skal vi gå gjennom hvordan du kan lese og behandle tekstfiler ved hjelp av Fish Shell.

## Slik gjør du det

For å lese en tekstfil ved hjelp av Fish Shell kan du bruke kommandoen `cat`. Denne kommandoen viser innholdet i en tekstfil på skjermen. For eksempel:

```Fish Shell
cat filnavn.txt
```

Dette vil vise innholdet i filen `filnavn.txt`. Du kan også bruke `less` kommandoen for å lese tekstfiler i en paginert visning. Dette er spesielt nyttig hvis filen er veldig lang.

```Fish Shell
less filnavn.txt
```

Dersom du vil lagre innholdet fra en tekstfil og behandle det videre, kan du bruke "redirect" operatøren `>` for å lagre det i en annen fil. For eksempel:

```Fish Shell
cat filnavn.txt > nyfil.txt
```

Dette vil lagre innholdet fra `filnavn.txt` i en ny fil som heter `nyfil.txt`.

## Dypdykk

Fish Shell har også innebygd støtte for å lese og behandle tekstfiler ved hjelp av variabler. For eksempel kan du bruke variabelen `$argv` for å få tilgang til argumentene som blir gitt til et Fish Shell-skript. Dette gjør det enklere å lese og behandle tekstfiler i mer komplekse skript.

En annen nyttig kommando er `grep` som lar deg søke etter et bestemt mønster eller ord i en tekstfil. For eksempel:

```Fish Shell
grep "fisk" filnavn.txt
```

Dette vil vise alle linjer i filen `filnavn.txt` som inneholder ordet "fisk".

## Se også

- Fish Shell dokumentasjonen: https://fishshell.com/docs/current/
- 10 tips for å gjøre Fish Shell enda mer kraftig: https://dev.to/charalampos197/10-tips-to-empower-fish-shell-41eg