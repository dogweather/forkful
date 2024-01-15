---
title:                "Oppretting av en midlertidig fil"
html_title:           "Haskell: Oppretting av en midlertidig fil"
simple_title:         "Oppretting av en midlertidig fil"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å lage midlertidige filer kan være svært nyttig når du jobber med programmeringsspråket Haskell. Det tillater deg å manipulere og lese/skrive til en fil uten å påvirke den permanente versjonen.

## Slik gjør du det

For å lage en midlertidig fil i Haskell, bruker du funksjonen `withSystemTempFile`. Denne funksjonen tar imot to parametere: en prefiks for filnavnet og en funksjon. Den midlertidige filen vil opprettes med et unikt navn basert på prefikset du gir. Den midlertidige filen vil automatisk bli slettet når funksjonen er ferdig med å kjøre.

```Haskell
import System.IO
import System.IO.Temp

withSystemTempFile "temp" $ \tempPath handle -> do
  putStrLn ("Opprettet midlertidig fil: " ++ tempPath) -- Output: Opprettet midlertidig fil: /var/folders/xz/87wdbd1d2bb4z
  hPutStrLn handle "Dette er innholdet i den midlertidige filen"
    
```

Innholdet du legger til i midlertidig fil vil bli slettet sammen med filen når `withSystemTempFile`-funksjonen er ferdig. Hvis du vil beholde innholdet, kan du bruke funksjonen `writeFile` for å skrive til filen.

```Haskell
withSystemTempFile "temp" $ \tempPath handle -> do
  writeFile tempPath "Dette er innholdet i den midlertidige filen"
  putStrLn "Filen er opprettet og innhold er skrevet til den"
```

## Dykk dypere

Hvis du ønsker mer kontroll over den midlertidige filen, kan du bruke funksjonen `withTempFile` i stedet for `withSystemTempFile`. Denne funksjonen tar imot en sti til en mappe og et prefiks for filnavnet.

```Haskell
import System.IO
import System.IO.Temp

withTempFile "C:\\temp" "temp" $ \tempPath handle -> do
  putStrLn ("Opprettet midlertidig fil: " ++ tempPath) -- Output: Opprettet midlertidig fil: C:\\temp\\temp.txt
  hPutStrLn handle "Dette er innholdet i den midlertidige filen"
```

Hvis du vil beholde den midlertidige filen etter at programmet er ferdig å kjøre, kan du bruke funksjonen `getTemporaryDirectory` for å finne stien til den midlertidige mappe og deretter flytte filen dit.

```Haskell
import System.Directory

getTemporaryDirectory >>= \tempDir -> do
  tempFile <- openTempFile tempDir "temp"
  putStrLn (fst tempFile) -- Output: /Users/username/AppData/Local/Temp/4138.temp
  hPutStrLn (snd tempFile) "Dette er innholdet i den midlertidige filen"
  renameFile (fst tempFile) "/Users/username/Dokumenter/temp.txt"
  putStrLn "Filen er nå flyttet og ikke lenger midlertidig"
```

## Se også

- [System.IO.Temp dokumentasjon](https://hackage.haskell.org/package/temp-1.2.3.1/docs/System-IO-Temp.html)
- [System.Directory dokumentasjon](https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html)