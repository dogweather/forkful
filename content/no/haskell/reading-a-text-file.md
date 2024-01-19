---
title:                "Lese en tekstfil"
html_title:           "C#: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å lese en tekstfil er prosessen å hente data direkte fra en fil i stedet for å skrive den inn manuelt. Programmører gjør dette for å lagre og gjenbruke store mengder data uten å gjenta koden.

## Hvordan:
Å lese en fil i Haskell er ganske enkelt. La oss se på et eksempel.

```Haskell
import System.IO   

hoved :: IO ()
hoved = gjøre
    innhold <- readFile "test.txt"
    putStrLn innhold
```

Ekte output kan være noe som ligner på følgende:

```Haskell
Dette er en test.
Hei, verden!
```

Dette eksemplet leser innholdet av `test.txt` filen og deretter skriver ut innholdet til konsollen.

## Dypdykk:
Historisk sett har programmeringsspråk alltid hatt nødvendigheten av å lese fra en fil, så dette er helt fra begynnelsen av informatikk. Haskell, derimot, ble etablert med en sterk vekt på funksjonell renhet, noe som gjør at operasjoner som fillesing, som er I/O-avhengige, blir håndtert litt annerledes enn andre språk.

Som alternativer kan du bruke `getContents` for å lese fra standard input, eller `Interact` funksjonen for å manipulere input og output på en høyere abstraksjonsnivå.

Når det gjelder implementeringsdetaljer, er `readfile` faktisk definert ved hjelp av `openFile` og `hGetContents`, som er de grunnleggende byggeklossene for fil IO i Haskell.

## Se også:
Studere disse kildene for å sakke perlefiske mer.

- [Haskell's IO inside](https://www.haskell.org/tutorial/io.html)
- [Chapter 7 - I/O](http://learnyouahaskell.com/input-and-output)
- [Real World Haskell - Files and handles](http://book.realworldhaskell.org/read/io.html)