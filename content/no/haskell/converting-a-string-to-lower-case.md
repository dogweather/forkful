---
title:                "Haskell: Konvertering av en streng til små bokstaver."
simple_title:         "Konvertering av en streng til små bokstaver."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange programmeringsoppgaver kan kreve å konvertere en streng (string) til små bokstaver (lower case) for å oppnå ønsket funksjonalitet. Det kan også være at man ønsker å samle data og analysere det på en mer nøyaktig måte ved å sørge for at alle bokstaver er i samme format. Uansett årsak, å kunne konvertere en streng til små bokstaver er en viktig og nyttig ferdighet i Haskell-programmering.

## Hvordan gjøre det
Først må vi forstå forskjellen mellom store og små bokstaver i Haskell. I Haskell betyr store bokstaver (upper case) at noe er en data type eller en konstruktør, mens små bokstaver (lower case) refererer til verdier eller funksjoner. Derfor, når vi ønsker å konvertere en streng til små bokstaver, må vi bruke en funksjon kalt `toLower` som er tilgjengelig i Haskell's `Data.Char` modul.

La oss se på et eksempel hvor vi ønsker å konvertere strengen "HELLO WORLD" til små bokstaver:

```Haskell
import Data.Char

toLowerString :: String -> String
toLowerString str = map toLower str

main = do
    let myString = "HELLO WORLD"
    putStrLn $ "Original streng: " ++ myString
    putStrLn $ "Konvertert streng: " ++ toLowerString myString
```

I første linje så importerer vi `Data.Char` modulen som inkluderer `toLower` funksjonen. Deretter definerer vi en funksjon `toLowerString` som har en streng som input og returnerer en streng. Inne i funksjonen gjør vi en mapping over `toLower` funksjonen på hver bokstav i input strengen. Til slutt, i `main` funksjonen, lager vi en variabel `myString` med vår originale streng og bruker `putStrLn` funksjonen til å skrive ut både original streng og den konverterte strengen ved å kalle `toLowerString` funksjonen.

La oss se på output av dette:

```
Original streng: HELLO WORLD
Konvertert streng: hello world
```

Som du kan se, har vi klart å konvertere vår streng til små bokstaver ved hjelp av `toLower` funksjonen. Dette eksempelet er en enkel måte å konvertere en streng til små bokstaver, men det er også mulig å gjøre det på en mer detaljert måte ved å bruke funksjoner som `toLower` og `shift`.

## Dykk dypere
Som nevnt tidligere, `toLower` funksjonen er tilgjengelig i `Data.Char` modulen og den konverterer en bokstav til små bokstaver. Men dersom du ønsker å konvertere en streng som inneholder ikke-alfabetiske tegn, vil funksjonen returnere den samme strengen siden den ikke kan konvertere ikke-alfabetiske tegn. For å håndtere dette, kan vi bruke funksjonen `shift` som også er tilgjengelig i `Data.Char` modulen. Denne funksjonen konverterer alle bokstaver til små bokstaver, uavhengig av om de er alfabetiske eller ikke.

La oss se på en modifisert versjon av vårt tidligere kodeeksempel som bruker `shift` funksjonen i stedet for `toLower` funksjonen:

```Haskell
import Data.Char

toLowerString :: String -> String
toLowerString str = map toLower str
```

Som du kan se, bruker vi fremdeles `toLower` funksjonen for å konvertere bokstaver til små bokstaver, men i dette tilfellet vil den returnere det samme som inputet siden det er ikke-alfabetiske tegn inne i strengen vår.

## Se også
- [Haskell Documentation for `Data.Char` module](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Char.html