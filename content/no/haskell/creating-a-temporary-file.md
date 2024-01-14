---
title:    "Haskell: Lage en midlertidig fil"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor
Mange ganger, i programmering, kan det være nødvendig å opprette midlertidige filer for å håndtere ulike data eller buffer. Dette kan være nyttig når du trenger å lagre data midlertidig mens du arbeider med et større prosjekt, eller når du trenger å skrive data til en fil før du sender den til en annen enhet. Opprettelse av midlertidige filer kan være en effektiv måte å organisere og håndtere data på, og i Haskell kan du gjøre dette ved hjelp av noen enkle funksjoner.

## Hvordan
Å opprette en midlertidig fil i Haskell er enkelt. Du må først importere "System.IO.Temp" -modulen og bruke funksjonen "withSystemTempFile". Dette vil opprette en midlertidig fil i systemets midlertidige katalog og returnere filbanen og håndtaket til filen. Se et eksempel nedenfor:

```Haskell
import System.IO.Temp

main = withSystemTempFile "tempFile.txt" $ \filepath handle -> do
  hPutStrLn handle "Dette er en midlertidig fil."
  putStrLn $ "Filbanen er: " ++ filepath
```

Dette vil skrive teksten til filen og skrive ut filbanen til midlertidig fil. Når programmet er ferdig, vil filen automatisk bli slettet fra systemet.

I tillegg kan du bruke funksjonen "withTempDirectory" for å opprette en midlertidig katalog i stedet for en fil. Dette kan være nyttig for å håndtere større mengder data eller filer.

```Haskell
import System.IO.Temp

main = withTempDirectory "tempDir" $ \dirpath -> do
  let filename = "tempFile.txt"
  let filepath = dirpath ++ "/" ++ filename
  writeFile filepath "Dette er en midlertidig fil i en midlertidig katalog."
  putStrLn $ "Filbanen er: " ++ filepath
```

## Dypdykk
Begge funksjonene "withSystemTempFile" og "withTempDirectory" tar også en "prefix"-parameter, som lar deg spesifisere et fornavn for filen eller katalogen som opprettes. Standardprefixen er "tmp", men du kan endre dette til noe som er mer meningsfullt for ditt program.

I tillegg kan du bruke "withTempFile" og "withTempDirectory" i stedet for "withSystemTempFile" og "withTempDirectory" hvis du vil spesifisere en annen katalog enn systemets midlertidige katalog. Dette kan være nyttig når du jobber med flere operativsystemer eller ønsker større kontroll over hvor filen/katalogen blir opprettet.

## Se Også
- Les mer om "System.IO.Temp" -modulen i Haskell-dokumentasjonen: https://hackage.haskell.org/package/temporary
- Utforsk andre nyttige moduler i Haskell for å arbeide med filer: https://hackage.haskell.org/packages/search?terms=file