---
title:                "Haskell: Skriving til standardfeil"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Hvorfor skrive til standardfeil i Haskell

Haskell er et funksjonelt programmeringsspråk som er kjent for å være strengt og typehåndtert. Det betyr at det er viktig å håndtere feil og unntak på en god måte for å sikre en robust og pålitelig kode. En måte å gjøre dette på er å skrive til standardfeil (stderr). Dette kan gi nyttig informasjon om eventuelle feil som oppstår under kjøring av programmet.

## Slik gjør du det

For å skrive til standardfeil i Haskell, kan du bruke funksjonen `hPutStrLn` fra modulen `System.IO`. Den tar inn en hånd som representerer standardfeil og en streng med meldingen du ønsker å skrive ut. For eksempel:

```haskell
import System.IO

main = do
  hPutStrLn stderr "Dette er en feilmelding"
```

Output vil være `Dette er en feilmelding` på standardfeil.

## Dykk dypere

Å skrive til standardfeil kan være spesielt nyttig når du jobber med feilhåndtering i Haskell. Det er vanlig å bruke `Either` eller `Maybe`-monader for å håndtere feil. Å skrive feilmeldinger til standardfeil kan hjelpe deg med å spore opp slike feil til riktig kontekst og løse dem.

En annen fordel med å skrive til standardfeil er at meldingen vil vises uavhengig av om den vanlige outputen blir omdirigert. Det gir deg mulighet til å se feilmeldinger selv om du logger eller skriver til en fil.

## Se også

- [Haskell-dokumentasjon om å skrive til standardfeil](https://hackage.haskell.org/package/base-4.15.2.0/docs/System-IO.html#v:hPutStrLn)
- [En grundig guide til feilhåndtering i Haskell](https://simonmar.github.io/bibifi-icfp-2012/tutorial/simonmar.pdf)
- [Offisiell Haskell-dokumentasjon](https://www.haskell.org/documentation/)