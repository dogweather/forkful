---
title:    "Haskell: Skriver til standardfeil"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standard error er en enkel måte å rapportere feil og advarsler i Haskell-programmering. Dette gjør det lettere å identifisere og løse problemer under utviklingsprosessen.

## Hvordan

For å skrive til standard error i Haskell, må du bruke funksjonene `hPutStr` eller `hPutStrLn` fra `System.IO` biblioteket. Dette vil la deg skrive en streng direkte til standard error.

```Haskell
import System.IO

main = do
  hPutStrLn stderr "Dette er en advarsel!"
```

Output vil vises i terminalen som:

```
Dette er en advarsel!
```

## Dykk dypere

Når du skriver til standard error, er det viktig å vite forskjellen mellom standard output og standard error. Standard output brukes vanligvis til å vise resultatene av et program, mens standard error brukes til å rapportere feil og advarsler. Ved å kommunisere på den riktige kanalen, blir det lettere å behandle og fange opp problemer under utviklingsprosessen.

En annen viktig ting å merke seg er at standard error kan omdirigeres til en fil ved hjelp av `2>` kommandoen i terminalen. Dette kan være nyttig når du kjører et program og ønsker å lagre eventuelle feilmeldinger til en fil for senere analyse.

## Se også

- [Haskell dokumentasjon for System.IO](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
- [Stack Overflow diskusjon om skriving til standard error i Haskell](https://stackoverflow.com/questions/20723936/writing-to-stderr-in-haskell)