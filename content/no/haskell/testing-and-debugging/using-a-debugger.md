---
date: 2024-01-26 03:49:24.935174-07:00
description: "\xC5 bruke en feils\xF8ker betyr \xE5 dykke inn i koden din med verkt\xF8\
  y designet for \xE5 inspisere, pause og manipulere et program midt i utf\xF8relsen.\
  \ Programmerere\u2026"
lastmod: '2024-03-13T22:44:40.846179-06:00'
model: gpt-4-0125-preview
summary: "\xC5 bruke en feils\xF8ker betyr \xE5 dykke inn i koden din med verkt\xF8\
  y designet for \xE5 inspisere, pause og manipulere et program midt i utf\xF8relsen.\
  \ Programmerere\u2026"
title: "\xC5 bruke en feils\xF8ker"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å bruke en feilsøker betyr å dykke inn i koden din med verktøy designet for å inspisere, pause og manipulere et program midt i utførelsen. Programmerere gjør dette for å jakte på feil, forstå programflyt og sørge for at koden deres gjør nøyaktig det de forventer.

## Hvordan:
La oss ta en tur med GHCi, Haskell sitt interaktive miljø som kan fungere som en grunnleggende feilsøker. Du fyrer det opp med Haskell-koden din og begynner å undersøke. Her er et eksempel:

```Haskell
main :: IO ()
main = do
    putStrLn "Hei, hva heter du?"
    navn <- getLine
    putStrLn $ "Hallo, " ++ navn ++ "! La oss feilsøke."
    let resultat = feilfunksjon 5
    print resultat

feilfunksjon :: Int -> Int
feilfunksjon n = n * 2 -- Lat som det er en feil her
```

For å starte feilsøking med GHCi:

```bash
$ ghci DinHaskellFil.hs
```

Sett et brytepunkt ved `feilfunksjon`:

```Haskell
Prelude> :break feilfunksjon
```

Kjør programmet ditt:

```Haskell
Prelude> :main
Hei, hva heter du?
```

Programmet ditt pauser ved `feilfunksjon`. Nå kan du inspisere variabler, gå gjennom koden steg for steg, og evaluere uttrykk.

## Dypdykk:
Historisk har Haskells rykte for rene funksjoner og sterk typetetthet ledet til troen på at feilsøkingsverktøy var mindre kritiske. Virkeligheten er annerledes—komplekse programmer drar alltid nytte av gode feilsøkingsverktøy. GHCi tilbyr grunnleggende feilsøkingskommandoer. Imidlertid, for en mer visuell opplevelse eller applikasjoner i større skala, kan du utforske IDEer med integrerte feilsøkere, som Visual Studio Code med Haskell-utvidelser eller IntelliJ sin Haskell-plugin.

Alternativer til feilsøker inkluderer bruk av utskriftssetninger, kjent som "printf-feilsøking," eller å utnytte Haskells sterke typesystem for å gjøre uriktige tilstander urepresentable. Likevel, ingenting erstatter noen ganger det å gå gjennom koden steg for steg.

Når det gjelder implementeringsdetaljer, fungerer Haskells feilsøker med kjøretidssystemet. Den kan håndtere brytepunkter, stegvis utførelse og tillate variabelinspeksjon. Imidlertid, siden Haskell er lat evaluerings, kan ting bli litt motintuitive. Å feilsøke et Haskell-program betyr ofte å holde et øye med når og hvordan uttrykk evalueres.

## Se Også:
- [GHC Brukerhåndbok - Feilsøker](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html)
- [IntelliJ Haskell-plugin](https://plugins.jetbrains.com/plugin/8258-intellij-haskell)
