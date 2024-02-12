---
title:                "Refaktorering"
aliases:
- /no/haskell/refactoring/
date:                  2024-01-26T01:37:54.894621-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktorering"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/refactoring.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Refaktorering er prosessen med å tweake koden din uten å endre dens eksterne oppførsel. Det handler alt om å rydde opp og organisere din handling for å gjøre koden enklere å lese, vedlikeholde og utvide. Det kan også bidra til å knuse bugs og forbedre ytelsen.

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
