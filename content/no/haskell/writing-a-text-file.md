---
title:                "Skrive en tekstdokument"
html_title:           "Haskell: Skrive en tekstdokument"
simple_title:         "Skrive en tekstdokument"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Så hvorfor skulle du engasjere deg i å skrive en tekstfil i Haskell? Vel, hvis du er en programmerer eller dataentusiast, så har du kanskje hørt om Haskell som et funksjonelt programmeringsspråk som er sterkt typet og hadde en kraftig typeinferensmotor. Det er også kjent for å være svært elegant og uttrykksfullt, noe som gjør det til en favoritt blant mange utviklere.

## Hvordan

For å begynne å skrive en tekstfil i Haskell, må du først åpne en fil for skriving ved å bruke `openFile` funksjonen. Deretter må du bestemme hvilken modus du vil skrive i ved å bruke `IOMode` datatype. For eksempel, hvis du vil skrive i enden av en eksisterende fil, kan du bruke `AppendMode`. Etter det kan du bruke `hPutStr` funksjonen til å skrive dataen din til filen. Her er et eksempel på hvordan koden kan se ut:

```Haskell
import System.IO

main = do
    handle <- openFile "filnavn.txt" AppendMode
    hPutStr handle "Dette er en tekstfil skrevet i Haskell"
    hClose handle
```

Output av denne koden vil være en tekstfil med innholdet "Dette er en tekstfil skrevet i Haskell".

## Dypdykk

En av de store fordelene med å skrive en tekstfil i Haskell er bruken av lazy evaluation. Dette betyr at filen ikke skrives til før du trenger å lese den. Dette er spesielt nyttig når du har med store filer å gjøre. Du kan også bruke `hPutStrLn` funksjonen for å skrive en ny linje til filen etter behov. Husk å alltid lukke filen din etter at du har skrevet til den ved å bruke `hClose` funksjonen.

## Se Også

- [Haskell's offisielle hjemmeside](https://www.haskell.org/)
- [Haskell Programmering Språk Wiki](https://wiki.haskell.org/Haskell)
- [Haskell Programmering Språk Tutorial](https://www.tutorialspoint.com/haskell/)