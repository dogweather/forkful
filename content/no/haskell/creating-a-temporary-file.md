---
title:                "Haskell: Lage en midlertidig fil"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å opprette midlertidige filer er en vanlig oppgave for mange programmører. Det kan være nyttig når du trenger å lagre midlertidige data eller som en del av en større prosess. I Haskell, kan det være nyttig å lære hvordan man oppretter midlertidige filer for å håndtere disse situasjonene.

## Hvordan

For å opprette en midlertidig fil i Haskell, kan du bruke funksjonen `withSystemTempFile` fra modulen `System.IO.Temp`. Her er et eksempel på hvordan du kan bruke den:

```Haskell
import System.IO.Temp (withSystemTempFile)
import System.IO (hPutStrLn, Handle, hClose)

main :: IO ()
main = withSystemTempFile "tempfile.txt" $ \tmpFile tmpHandle -> do 
    hPutStrLn tmpHandle "Dette er en midlertidig fil."
    hPutStrLn tmpHandle "Denne filen vil bli slettet når programmet avsluttes."
    hClose tmpHandle
    putStrLn $ "Filen ble opprettet på denne plassen: " ++ tmpFile
```

I dette eksempelet opprettes en midlertidig fil med navnet `tempfile.txt` ved hjelp av `withSystemTempFile`. Deretter blir filen brukt til å skrive ut to setninger ved hjelp av `hPutStrLn`. Til slutt lukkes fila og vi får en melding som viser hvor filen ble opprettet.

## Dypdykk

`withSystemTempFile` funksjonen returnerer en `IO` handling som tar en filbane og en `Handle` som argumenter. Den midlertidige filen vil bli opprettet på den gitte filbanen, og `Handle` vil bli brukt til å skrive data til filen.

Når `withSystemTempFile` er ferdig, blir `Handle` lukket og filen blir automatisk slettet. Dette er nyttig for å unngå rotete filer som ikke lenger trengs.

## Se Også
- [Haskell Dokumentasjon - System.IO.Temp](https://hackage.haskell.org/package/base/docs/System-IO-Temp.html)
- [Haskell Programmering fra Bunnen av - Kapittel 9: Filbehandling](https://www.ufundu.no/haskell-kap9.html)