---
title:                "Elm: Opprettelse av en midlertidig fil"
simple_title:         "Opprettelse av en midlertidig fil"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lage midlertidige filer er en vanlig oppgave i programmering, og kan være nyttig for å lagre data midlertidig eller for å dele data mellom forskjellige programmer. I denne bloggen vil vi vise deg hvordan du kan opprette midlertidige filer ved hjelp av det funksjonelle og enkle programmeringsspråket, Elm.

## Hvordan

For å opprette midlertidige filer i Elm, må vi først importere `File` biblioteket. Deretter bruker vi `File.system` funksjonen for å opprette en fil og gi den et navn.

```
Elm importera: File

file = File.system "tmp.txt"
```

Vi kan også spesifisere hvilken mappe filen skal opprettes i ved å legge til en sti som et ekstra argument i `File.system` funksjonen.

```
file = File.system "tmp.txt" "tmpFolder"
```

Vi kan også legge til innhold i filen ved å bruke `File.write` funksjonen.

```
file = File.system "tmp.txt"
File.write file "Dette er innholdet i filen."
```

For å lagre endringene og faktisk opprette filen, bruker vi `File.save` funksjonen.

```
file = File.system "tmp.txt"
File.write file "Dette er innholdet i filen."
File.save file
```

Etter at filen er opprettet, kan vi bruke `File.read` funksjonen for å lese innholdet i filen.

```
file = File.system "tmp.txt"
innhold = File.read file
```

I dette tilfellet vil `innhold` variabelen inneholde strengen "Dette er innholdet i filen."

## Dypdykk

Når vi oppretter en midlertidig fil, er det viktig å huske at denne filen vil bli slettet når programmet termineres. Dette skjer fordi filen er opprettet i systemets midlertidige mappe. Hvis vi vil beholde filen for lengre tid, må vi flytte den til et annet sted før programmet termineres.

Det kan også være nyttig å vite at filnavnet som blir generert av `File.system` funksjonen er tilfeldig, slik at det ikke vil sammenfalle med eksisterende filer i systemet.

## Se også

- [Elm Dokumentasjon om `File` biblioteket](https://package.elm-lang.org/packages/elm/file/latest/)
- [Elm Programmeringsspråkets Offisielle Nettsted](https://elm-lang.org/)