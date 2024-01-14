---
title:    "Haskell: Skriver en tekstfil"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Hvorfor

Å skrive en tekstfil kan virke som en enkel oppgave, men det er faktisk en veldig nyttig ferdighet å ha som programmerer. Det kan hjelpe deg med å lagre og organisere informasjon, samt å automatisere prosesser. I tillegg kan det være nyttig når du jobber med større programmeringsprosjekter.

## Hvordan
For å skrive en tekstfil i Haskell, kan du bruke en funksjon kalt `writeFile`. Denne funksjonen tar inn en filsti og en string som argumenter, og skriver stringen til filen på den angitte filstien. Her er et eksempel på hvordan du bruker `writeFile` i Haskell:

```
writeFile "example.txt" "Dette er en tekstfil skrevet i Haskell."
```

Denne koden vil lage en fil kalt "example.txt" og skrive stringen "Dette er en tekstfil skrevet i Haskell." til filen.

## Dypdykk
Det finnes flere forskjellige måter å skrive en tekstfil på i Haskell, inkludert å bruke `putStrLn` og `print` funksjoner. Disse funksjonene kan brukes til å skrive informasjon til en fil ved å sende den til terminalen. I tillegg kan du også bruke ulike filbehandlingsfunksjoner som `openFile` og `closeFile` for å skrive til og lukke en tekstfil. Det er også mulig å lese og manipulere tekstfiler i Haskell ved å bruke funksjoner som `readFile` og `lines`.

## Se også
- [Write to a file in Haskell](https://www.tutorialspoint.com/write-to-a-file-in-haskell)
- [Haskell File I/O](https://ryanmccarthy.gitlab.io/haskell-file-io/)
- [File handling in Haskell](https://wiki.haskell.org/File_handling)