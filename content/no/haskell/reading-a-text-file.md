---
title:    "Haskell: Å lese en tekstfil"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Haskell er et kraftig programmeringsspråk som er kjent for sin funksjonelle og elegante kode. En av de mange nyttige funksjonene er evnen til å lese tekstfiler, noe som kan være svært nyttig i mange programmeringsscenarier. Les videre for å lære hvordan du kan lese en tekstfil i Haskell.

## Hvordan

For å lese en tekstfil i Haskell, trenger du først å åpne filen ved hjelp av `openFile` -funksjonen. Denne funksjonen tar to argumenter, navnet på filen og modus for å åpne filen. Modusen kan være "ReadMode" for å lese en fil, "WriteMode" for å skrive til en fil eller "AppendMode" for å legge til i en eksisterende fil.

```Haskell
import System.IO

main = do
   fil <- openFile "tekstfil.txt" ReadMode
```

Etter at filen er åpnet, kan du bruke `hGetContents` -funksjonen for å få innholdet i filen. Denne funksjonen returnerer en streng som inneholder hele teksten i filen.

```Haskell
import System.IO

main = do
   fil <- openFile "tekstfil.txt" ReadMode
   innhold <- hGetContents fil
```

For å vise innholdet i filen, kan du bruk `putStrLn` -funksjonen.

```Haskell
import System.IO

main = do
   fil <- openFile "tekstfil.txt" ReadMode
   innhold <- hGetContents fil
   putStrLn innhold
```

## Dypdykk

Når du har åpnet filen og fått innholdet, er det viktig å lukke filen igjen ved hjelp av `hClose` -funksjonen. Dette vil frigjøre eventuelle ressurser som ble brukt til å åpne filen.

```Haskell
import System.IO

main = do
   fil <- openFile "tekstfil.txt" ReadMode
   innhold <- hGetContents fil
   putStrLn innhold
   hClose fil
```

Hvis du ønsker å lese en tekstfil linje for linje, kan du bruke `hGetLine` -funksjonen. Denne funksjonen returnerer en streng som inneholder en enkelt linje fra filen.

```Haskell
import System.IO

main = do
   fil <- openFile "tekstfil.txt" ReadMode
   linje1 <- hGetLine fil
   linje2 <- hGetLine fil
   putStrLn linje1
   putStrLn linje2
   hClose fil
```

## Se også

- [Lesing av tekstfiler i Haskell](https://www.tutorialspoint.com/haskell/haskell_input_output.htm)
- [Offisiell dokumentasjon for System.IO](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
- [Filbehandling i Haskell](https://wiki.haskell.org/Dealing_with_files)