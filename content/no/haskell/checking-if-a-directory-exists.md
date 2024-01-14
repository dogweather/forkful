---
title:                "Haskell: Sjekke om en mappe eksisterer"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Hvorfor?

I programmering er det ofte nødvendig å sjekke om en mappe eksisterer før man kan utføre handlinger på den. Dette kan være nyttig for å unngå feil og sikre at koden kjører som forventet. I denne bloggposten vil vi gå gjennom hvordan man kan sjekke om en mappe finnes i Haskell.

## Hvordan gjøre det

For å sjekke om en mappe eksisterer i Haskell, kan man bruke funksjonen `doesDirectoryExist` fra modulen `System.Directory`. Denne funksjonen tar inn en streng med stien til mappen man ønsker å sjekke, og returnerer en boolsk verdi som indikerer om mappen eksisterer eller ikke. La oss se på et eksempel:

```Haskell
import System.Directory

main = do
  exists <- doesDirectoryExist "mappen/min"
  if exists
    then putStrLn "Mappen eksisterer"
    else putStrLn "Mappen eksisterer ikke"
```

I dette eksempelet bruker vi `doesDirectoryExist` til å sjekke om mappen "mappen/min" finnes. Dersom den finnes, vil programmet skrive ut "Mappen eksisterer", ellers vil det skrive ut "Mappen eksisterer ikke". Enkel og grei måte å sikre at koden vår fungerer som den skal!

## Dykk dypere

Men hva skjer egentlig bak kulissene når vi kaller på `doesDirectoryExist`? I Haskell er det vanlig å bruke et konsept kalt "monads", som kan hjelpe oss å håndtere asynkrone eller potensielt feilfulle operasjoner, som for eksempel å sjekke om mappen vår eksisterer. Når vi bruker funksjonen `doesDirectoryExist`, kjøres operasjonen i en monad som håndterer eventuelle feil som kan oppstå. Dette kan kanskje virke litt komplisert, men det er faktisk ganske praktisk og effektivt når man blir vant til det.

## Se også

- [Dokumentasjon for System.Directory](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Haskell monads for nybegynnere](https://blog.ssanj.net/posts/2014-09-23-A-Simple-Intro-to-Monads-for-Haskell-Programmers.html)
- [En guide til Haskell for nybegynnere](https://www.haskell.org/documentation/)