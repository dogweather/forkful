---
title:                "Opprette en midlertidig fil"
date:                  2024-01-20T17:40:24.344850-07:00
model:                 gpt-4-1106-preview
simple_title:         "Opprette en midlertidig fil"

category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?
Oppretting av en midlertidig fil lar programmer lagre data som trengs for kort tid. Vi gjør dette for å håndtere data som ikke trenger å bli varig lagret eller når vi vil unngå å påvirke programmets tilstand eller ytelse.

## How to:
```Haskell
import System.IO.Temp (withSystemTempFile)
import System.IO (hPutStrLn, hGetContents)

main :: IO ()
main = withSystemTempFile "mytemp.txt" $ \path handle -> do
    putStrLn $ "Filen er opprettet: " ++ path
    hPutStrLn handle "Noe midlertidig innhold"
    -- husk å lukke filen for å lagre innholdet
    contents <- hGetContents handle
    putStrLn $ "Innholdet i filen: " ++ contents
    -- filen fjernes automatisk her
```
Kjøre eksempel:
```
Filen er opprettet: /tmp/mytemp.txt1234
Innholdet i filen: Noe midlertidig innhold
```

## Deep Dive
Midlertidige filer er ikke nytt. De har vært brukt siden tidlige dagers databehandling for å håndtere ekstra data uten å tære på primær lagring. I Haskell, `System.IO.Temp` modulen tilbyr funksjoner for å håndtere midlertidige filer. `withSystemTempFile` funksjonen er en høyereordens funksjon som tar seg av både opprettelse og sletting av midlertidige filer, og sørger for sikkerhet da andre brukere ikke kan aksessere filen mens den er åpen. Alternativene inkluderer å bruke lavnivå IO operasjoner for å manuelt håndtere filer, eller å bruke biblioteker som `temporary` for mer funksjonalitet. Implementasjonen er avhengig av systemkall for å sikre unikhet og sikkerhet av den midlertidige filen.

## See Also
- Haskell `System.IO.Temp` dokumentasjon: [hackage.haskell.org/package/temporary](https://hackage.haskell.org/package/temporary-1.3/docs/System-IO-Temp.html)
- `temporary` Haskell pakke: [hackage.haskell.org/package/temporary](https://hackage.haskell.org/package/temporary)
