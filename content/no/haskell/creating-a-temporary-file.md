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

## Hva & Hvorfor?
Opprettelse av midlertidige filer er en viktig del av programmering, spesielt når det kommer til å håndtere data og lagre midlertidig informasjon. Dette er nyttig når man trenger å behandle data som ikke skal lagres permanent, eller når man ønsker å teste kode uten å endre den eksisterende filen. Det er også en måte å sikre at data blir ryddet opp når det ikke lenger er behov for dem.

## Hvordan:
```Haskell
import System.IO.Temp (withSystemTempFile)

main :: IO ()
main = withSystemTempFile "sample.txt" $ \tmpFile handle -> do
  putStrLn $ "Temp file created at: " ++ tmpFile
  hPutStrLn handle "Sample data"
```
Output:
```
Temp file created at: /var/folders/mn/b_h801bx36d7qpxnh2b6096m0000gn/T/sample.txt
```
Først importerer vi `withSystemTempFile` fra `System.IO.Temp` modulen. Deretter bruker vi funksjonen som tar imot to parametere - et navn til den midlertidige filen og en funksjon som tar imot plasseringen til den midlertidige filen og en håndterer til filen. Inne i funksjonen kan vi gjøre operasjoner på filen, som å skrive eller lese data. Når funksjonen er ferdig, vil filen automatisk bli slettet.

## Dypdykk:
Opprettelse av midlertidige filer har blitt brukt i programmering i lang tid, da det har vært enklere å lagre data midlertidig i en fil enn å håndtere dem i minnet. Alternativet til å opprette en midlertidig fil er å bruke variabler eller lister, men dette kan være mindre effektivt og ikke alltid mulig.

I Haskell, kan vi også bruke `System.IO.Temp` modulen til å opprette en midlertidig katalog ved hjelp av `withSystemTempDirectory` funksjonen. Dette kan være nyttig for å lagre midlertidige filer som er relatert til hverandre.

Når det kommer til implementering, bruker Haskell `withSystemTempFile` funksjonen `openTempFile` under panseret, som i utgangspunktet gjør akkurat det samme, bortsett fra at det returnerer en håndterer til filen i stedet for plasseringen.

## Se også:
- [Haskell documentation: System.IO.Temp](https://hackage.haskell.org/package/temporary-1.3/docs/System-IO-Temp.html)
- [Using Temporary Files in Haskell](https://mmhaskell.com/blog/2017/5/15/using-temporary-files-in-haskell)