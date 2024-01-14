---
title:    "Haskell: Å skrive til standardfeil"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standard error kan være nyttig når du vil gi beskjed om feil eller uforutsette hendelser i et Haskell-program. Ved å skrive disse meldingene til standard error i stedet for standard output, kan du enklere skille dem fra andre utdata. 

## Hvordan

For å skrive til standard error i Haskell, kan du bruke funksjonen `hPrint` fra `System.IO`-modulen. Denne funksjonen tar inn en håndterer (`Handle`) som representerer standard error, og en verdi eller streng du vil skrive.

```Haskell
import System.IO (stderr, hPrint)

main = do 
  hPrint stderr "Dette er en melding til standard error"
```

Output av dette programmet vil se slik ut når det kjøres:

```
Dette er en melding til standard error
```

Merk at meldingen skrives ut til standard error i stedet for standard output. Dette gjør det enklere å skille mellom ulike typer utdata.

## Dypdykk

I Haskell er standard error håndtert av en global håndterer som kan tilgjengeliggjøres ved hjelp av funksjonen `stderr`. Denne funksjonen tar ikke inn noen parametere og returnerer en håndterer til standard error. 

Det er også mulig å endre standard error håndtereren til en annen håndterer ved hjelp av funksjonen `hSetStrerr` fra `System.IO`-modulen. Denne funksjonen tar inn en håndterer som du ønsker å bruke som standard error, og endrer dermed standard error kanalen for resten av programmet. 

Å skrive til standard error kan også hjelpe deg med å feilsøke programmer, siden meldingene du skriver ut her vil vises på en tydeligere måte. Det er også mulig å fange og behandle disse meldingene på en mer avansert måte ved hjelp av Haskell sine error-handteringsmekanismer.

## Se også

* [Skriver til filer i Haskell](https://www.codementor.io/@michaelnguyen212/how-to-write-to-a-file-in-haskell-1k3xz3ctyf)
* [System.IO-modulen i Haskell](https://hackage.haskell.org/package/base/docs/System-IO.html)
* [Hvordan håndtere feil i Haskell](https://www.educative.io/blog/handling-errors-in-haskell)