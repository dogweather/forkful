---
date: 2024-01-20 18:04:14.762933-07:00
description: "\xC5 starte et nytt prosjekt handler om \xE5 skape en ren skifer for\
  \ \xE5 utvikle programvare. Programmerere gj\xF8r dette for \xE5 realisere nye ideer,\
  \ l\xF8se problemer,\u2026"
lastmod: 2024-02-19 22:05:00.106549
model: gpt-4-1106-preview
summary: "\xC5 starte et nytt prosjekt handler om \xE5 skape en ren skifer for \xE5\
  \ utvikle programvare. Programmerere gj\xF8r dette for \xE5 realisere nye ideer,\
  \ l\xF8se problemer,\u2026"
title: "\xC5 starte et nytt prosjekt"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å starte et nytt prosjekt handler om å skape en ren skifer for å utvikle programvare. Programmerere gjør dette for å realisere nye ideer, løse problemer, eller teste ut nye teknologier.

## Slik gjør du:
For å sparke i gang et Haskell-prosjekt, starter vi med Stack, et bygge- og prosjektverktøy som gjør livet lettere. 

Installer Stack om du ikke har det:
```shell
curl -sSL https://get.haskellstack.org/ | sh
```

Nå, la oss lage et nytt prosjekt:
```shell
stack new mittprosjekt
```

Koden over hiver deg inn i en verden av Haskell med et standardoppsett. `mittprosjekt` kan være hva som helst du ønsker.

For å bygge prosjektet ditt, naviger til prosjektmappe og kjør:
```shell
cd mittprosjekt
stack build
```

Kjør programmet med:
```shell
stack exec mittprosjekt-exe
```

Du burde se noe ala:
```
Noe flott output fra ditt Haskell-program!
```

## Dypdykk
Haskell ble sluppet løs i 1990, bygd for funksjonell programmering. Stack kom derimot i 2014 for å gjøre Haskell-håndtering enklere. Alternativer inkluderer Cabal, Haskells originale byggesystem, men Stack vinner på brukervennlighet.

Når du starter et prosjekt, setter Stack opp et byggemiljø som håndterer avhengigheter og kompilering uten hodebry. Det bruker en kurert sett av pakker kalt Stackage for å unngå "dependency hell". 

Stack og project templates hjelper deg å hoppe over tørr koden og rett på det gøyale – skape noe nytt.

## Se også
- [Official Stack documentation](https://docs.haskellstack.org/en/stable/README/)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
- [Haskell Programming from First Principles](http://haskellbook.com/)
- [The Haskell Tool Stack Subreddit](https://www.reddit.com/r/haskellstack/)
