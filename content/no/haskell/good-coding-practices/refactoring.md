---
date: 2024-01-26 01:37:54.894621-07:00
description: "Hvordan: La oss si at du har en klump med Haskell-kode som gjentar seg\
  \ selv mer enn din favorittsang. Her er en rask titt p\xE5 hvordan du kan refaktorere\u2026"
lastmod: '2024-03-13T22:44:40.850271-06:00'
model: gpt-4-0125-preview
summary: La oss si at du har en klump med Haskell-kode som gjentar seg selv mer enn
  din favorittsang.
title: Refaktorering
weight: 19
---

## Hvordan:
La oss si at du har en klump med Haskell-kode som gjentar seg selv mer enn din favorittsang. Her er en rask titt på hvordan du kan refaktorere det ved å bruke funksjoner.

Før refaktorering:

```haskell
printInvoice :: String -> Float -> String -> IO ()
printInvoice kunde total vare = do
  putStrLn $ "Kunde: " ++ kunde
  putStrLn $ "Total: " ++ show total
  putStrLn $ "Vare: " ++ vare
```

Etter litt refaktorering:

```haskell
printDetail :: String -> String -> IO ()
printDetail etikett verdi = putStrLn $ etikett ++ ": " ++ verdi

printInvoice :: String -> Float -> String -> IO ()
printInvoice kunde total vare = do
  printDetail "Kunde" kunde
  printDetail "Total" (show total)
  printDetail "Vare" vare

-- Eksempel på utdata:
-- Kunde: Alice
-- Total: $42.00
-- Vare: Haskell Programmeringsguide
```

Som du kan se, ved å trekke ut det felles mønsteret i en separat `printDetail`-funksjon, unngår vi repetisjon og gjør `printInvoice` klarere og enklere å håndtere.

## Dykk dypere
Da Haskell kom på scenen på slutten av 80-tallet, var det klart at det funksjonelle paradigmet kunne bringe litt frisk luft til kodningspraksiser. Hurtig framover, og refaktorering i Haskell er spesielt elegant takket være at funksjoner er statsborgere av første klasse og dens sterke statiske typesystem. Du refaktorerer uten frykt for at du skulle ødelegge appen din, siden kompilatoren har ryggen din.

Alternativer til manuell refaktorering kan inkludere å bruke automatiserte verktøy, selv om den funksjonelle naturen og typesikkerheten til Haskell noen ganger kan gjøre dette mindre utbredt sammenlignet med andre språk. Implementeringsmessig er det viktig å utnytte Haskells funksjoner som høyere ordens funksjoner, renhet og uforanderlighet for å gjøre refaktoreringen jevnere.

Refaktoreringer som "Ekstrakt Funksjon", nettopp vist, er vanlig, men du kan også gjøre "Sett inn Funksjon", "Endre Variabelnavn" og "Endre Funksjonssignatur" med tillit, takket være typesystemet. Haskells kraftige typeinferens kan noen ganger oppdage feil som ville gli gjennom i andre språk.

## Se også
For et dypdykk inn i refaktorering i Haskell, slå opp bøkene med "Refactoring: Improving the Design of Existing Code" av Martin Fowler, hvor konseptene er universelt anvendbare. Sjekk ut hlint-verktøyet for automatiserte hint om å forbedre din Haskell-kode. Også, stikk innom Haskell wiki (https://wiki.haskell.org/Refactoring) for fellesskapets innsikter og videre lesning.
