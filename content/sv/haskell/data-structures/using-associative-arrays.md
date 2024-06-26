---
changelog:
- 2024-01-30, dogweather, reviewed
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:16.726260-07:00
description: "Hur g\xF6r man: Haskell har inte associerande arrayer direkt ur l\xE5\
  dan p\xE5 samma s\xE4tt som vissa andra spr\xE5k, men det erbjuder ett kraftfullt\
  \ standardbibliotek\u2026"
lastmod: '2024-03-13T22:44:37.948319-06:00'
model: gpt-4-0125-preview
summary: "Haskell har inte associerande arrayer direkt ur l\xE5dan p\xE5 samma s\xE4\
  tt som vissa andra spr\xE5k, men det erbjuder ett kraftfullt standardbibliotek kallat\
  \ `Data.Map` f\xF6r att arbeta med nyckel-v\xE4rde-par."
title: "Att anv\xE4nda associativa arrayer"
weight: 15
---

## Hur gör man:
Haskell har inte associerande arrayer direkt ur lådan på samma sätt som vissa andra språk, men det erbjuder ett kraftfullt standardbibliotek kallat `Data.Map` för att arbeta med nyckel-värde-par. Låt oss kavla upp ärmarna och se hur man använder dem!

Först, se till att importera det:
```Haskell
import qualified Data.Map as Map
```

Att skapa en karta är enkelt. Låt oss skapa en med några programmeringsspråk och deras paradigmer:
```Haskell
let languages = Map.fromList [("Haskell", "Funktionell"), ("Python", "Imperativ"), ("Prolog", "Logisk")]
```

Nu, hur får vi reda på paradigmet för Haskell?
```Haskell
Map.lookup "Haskell" languages
-- utdata: Just "Funktionell"
```

Att lägga till ett nytt språk är enkelt:
```Haskell
let languagesUpdated = Map.insert "Rust" "System" languages
```

Vad om vi vill lista alla språk? Använd `Map.keys`:
```Haskell
Map.keys languagesUpdated
-- utdata: ["Haskell","Python","Prolog","Rust"]
```

För att lista paradigmer, använd `Map.elems`:
```Haskell
Map.elems languagesUpdated
-- utdata: ["Funktionell","Imperativ","Logisk","System"]
```

Dessa grundläggande operationer bör täcka de flesta användningsfall, men det finns mycket mer att utforska i `Data.Map`!

## Djupdykning
`Data.Map`-modulen i Haskells standardbibliotek är byggd ovanpå balanserade binärträd, specifikt AVL-träd. Detta val säkerställer att de flesta operationer på kartan, såsom insättning, borttagning och uppslagning, kan göras på O(log n) tid, där n är antalet element i kartan. Det är ett effektivt val för många användningsfall, även om det inte är det absolut snabbaste för alla scenarier.

Det finns en historisk nyans också: innan `Data.Map` blev det självklara valet, använde Haskell-programmerare ofta listor av par för att simulera associerande arrayer. Däremot, operationer på sådana strukturer är O(n) för uppslagning, vilket gjorde `Data.Map` till en betydande förbättring i termer av prestanda.

Nu, trots effektiviteten och användbarheten hos `Data.Map`, är det inte alltid det bästa verktyget för varje uppgift. För uppgifter som är mycket prestandakänsliga, där även O(log n) uppslagningstider är för långsamma, eller där nycklar alltid är heltalsvärden, kan arrays eller hashtabeller (via `Data.HashMap`) erbjuda bättre prestanda med O(1) åtkomsttider.

Haskell-ekosystemet erbjuder en mängd olika datastrukturer för att passa olika behov, och `Data.Map` är ett utmärkt allmängiltigt val för associerande arrayer, som balanserar användarvänlighet, flexibilitet och prestanda.
