---
date: 2024-01-20 17:40:24.344850-07:00
description: 'How to: .'
lastmod: '2024-03-13T22:44:40.862751-06:00'
model: gpt-4-1106-preview
summary: .
title: Opprette en midlertidig fil
weight: 21
---

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
