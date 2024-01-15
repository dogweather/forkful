---
title:                "Skriver til standardfeil"
html_title:           "Haskell: Skriver til standardfeil"
simple_title:         "Skriver til standardfeil"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standard error kan være nyttig for å få mer informasjon om feil i programmering. Det lar deg få informasjon om hva som gikk galt og hvor i koden det skjedde, noe som kan være nyttig for å finne og rette feil.

## Slik gjør du det

For å skrive til standard error i Haskell, kan du bruke funksjonen `hPutStrLn` fra `System.IO`-modulen. Denne funksjonen tar inn en handle, som i dette tilfellet er `stderr`, og en streng som skal skrives ut. Her er et eksempel på hvordan du kan bruke den:

```Haskell
import System.IO (hPutStrLn, stderr)

main = do
  hPutStrLn stderr "Dette er en feilmelding"
```

Dette vil skrive ut teksten "Dette er en feilmelding" til standard error. Merk at du må importere modulen `System.IO` for å bruke `hPutStrLn`-funksjonen.

## Dykk dypere

Det finnes også andre funksjoner i `System.IO`-modulen som kan være nyttige for å skrive til standard error. For eksempel kan du bruke `hPrintf` for å skrive ut formatert tekst til handle. Du kan også bruke `hPutStr` eller `hPutChar` for å skrive ut uten ny linje, eller `hFlush` for å tømme buffere og få teksten til å vises umiddelbart.

Det kan også være nyttig å vite at du kan definere din egen handle til standard error ved hjelp av `stderr :: Handle`. Dette kan være nyttig hvis du vil ha en egen handle spesifikt for å skrive til standard error.

## Se også

- [Offisiell dokumentasjon for `System.IO`-modulen](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
- [Haskell Programmering fra Scratch av Timothy Renner](https://www.simplesnippets.tech/haskell-for-beginners/)