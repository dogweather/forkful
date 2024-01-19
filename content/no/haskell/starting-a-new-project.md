---
title:                "Å starte et nytt prosjekt"
html_title:           "C: Å starte et nytt prosjekt"
simple_title:         "Å starte et nytt prosjekt"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å starte et nytt prosjekt er prosessen med å opprette en grunnstruktur for en ny applikasjon. Programmerere gjør det for å legge grunnlaget for utviklingen med en god struktur og spesialtilpasset oppsett.

## Hvordan:

La oss lage en enkel "Hello, World!"-program i Haskell. 

```Haskell
main :: IO ()
main = putStrLn "Hello, World!"
```

Når du kjører denne koden, vil output være:

```
Hello, World!
```

Dette er grunnleggende Haskell-kode for å skrive ut en streng til konsollen.

## Dyp Dykk:

Historisk, Haskell ble utviklet i 1990 for å hindre redundans og øke kodeeffektiviteten. Det er mange alternativer til Haskell, som Python, Java, og C++, men Haskell er kjent for sin sterke statiske type system, late evaluering, rik uttrykkskraft og klar syntaks.

Et prosjekt i Haskell involverer vanligvis flere filer, moduler og pakkedirektiver. Prosjektstrukturen hjelper deg å organisere koden din bedre og gjør den enklere å vedlikeholde.

## Se Også:

1. Hvis du er ny til Haskell, ta en titt på [Learn You a Haskell](http://learnyouahaskell.com/chapters) for en omfattende tutorial.
2. For mer komplekse eksempler og "beste praksis", besøk [Haskell's offisielle Wikibook](https://en.wikibooks.org/wiki/Haskell).
3. Skjekk ut [Haskell's offisielle dokumentasjon](https://www.haskell.org/documentation) for omfattende detaljer om språket og dets biblioteker.