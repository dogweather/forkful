---
changelog:
- 2024-01-30, dogweather, reviewed
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:38.135493-07:00
description: "Hvordan: Haskell har ikke assosiative tabeller rett ut av boksen p\xE5\
  \ samme m\xE5te som noen andre spr\xE5k, men det tilbyr et kraftfullt standardbibliotek\
  \ kalt\u2026"
lastmod: '2024-03-13T22:44:40.834141-06:00'
model: gpt-4-0125-preview
summary: "Haskell har ikke assosiative tabeller rett ut av boksen p\xE5 samme m\xE5\
  te som noen andre spr\xE5k, men det tilbyr et kraftfullt standardbibliotek kalt\
  \ `Data.Map` for \xE5 arbeide med n\xF8kkel-verdi-par."
title: Bruke associative tabeller
weight: 15
---

## Hvordan:
Haskell har ikke assosiative tabeller rett ut av boksen på samme måte som noen andre språk, men det tilbyr et kraftfullt standardbibliotek kalt `Data.Map` for å arbeide med nøkkel-verdi-par. La oss brette opp ermene og se hvordan vi bruker dem!

Først, sørg for å importere det:
```Haskell
import qualified Data.Map as Map
```

Å opprette et kart er enkelt. La oss opprette ett med noen programmeringsspråk og deres paradigmer:
```Haskell
let languages = Map.fromList [("Haskell", "Funksjonelt"), ("Python", "Imperativt"), ("Prolog", "Logisk")]
```

Nå, hva med å få paradigmet til Haskell?
```Haskell
Map.lookup "Haskell" languages
-- utdata: Just "Funksjonelt"
```

Å legge til et nytt språk er enkelt:
```Haskell
let languagesUpdated = Map.insert "Rust" "Systemer" languages
```

Hva om vi vil liste alle språkene? Bruk `Map.keys`:
```Haskell
Map.keys languagesUpdated
-- utdata: ["Haskell","Python","Prolog","Rust"]
```

For å liste paradigmer, bruk `Map.elems`:
```Haskell
Map.elems languagesUpdated
-- utdata: ["Funksjonelt","Imperativt","Logisk","Systemer"]
```

Disse grunnleggende operasjonene bør dekke de fleste bruk, men det er mye mer å utforske i `Data.Map`!

## Dypdykk
`Data.Map`-modulen i Haskells standardbibliotek er bygget på balanserte binære trær, spesifikt AVL-trær. Dette valget sikrer at de fleste operasjoner på kartet, som innsetting, sletting og oppslag, kan gjøres på O(log n) tid, hvor n er antall elementer i kartet. Det er et effektivt valg for mange brukstilfeller, selv om det ikke er det absolutt raskeste for alle scenarier.

Det er også en historisk nyanse: før `Data.Map` ble gå-til-valget, brukte Haskell-programmerere ofte lister av par for å simulere assosiative tabeller. Men operasjoner på slike strukturer er O(n) for oppslag, noe som gjør `Data.Map` til en betydelig forbedring i ytelsen.

Nå, til tross for effektiviteten og nytten av `Data.Map`, er det ikke alltid det beste verktøyet for hver jobb. For svært ytelsessensitive oppgaver, hvor selv O(log n) oppslagstider er for langsomme, eller hvor nøkler alltid er heltallsverdier, kan tabeller eller hashtabeller (via `Data.HashMap`) tilby bedre ytelse med O(1) tilgangstider.

Haskell-økosystemet tillater en rekke datastrukturer for å passe ulike behov, og `Data.Map` er et utmerket valg med generelt formål for assosiative tabeller, som balanserer brukervennlighet, fleksibilitet og ytelse.
