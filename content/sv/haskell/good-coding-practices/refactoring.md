---
date: 2024-01-26 01:18:41.615149-07:00
description: "Hur man g\xF6r: L\xE5t oss s\xE4ga att du har ett stycke Haskell-kod\
  \ som upprepar sig mer \xE4n din favoritl\xE5t. H\xE4r \xE4r en snabb titt p\xE5\
  \ hur du kan refaktorisera det\u2026"
lastmod: '2024-03-13T22:44:37.963161-06:00'
model: gpt-4-0125-preview
summary: "L\xE5t oss s\xE4ga att du har ett stycke Haskell-kod som upprepar sig mer\
  \ \xE4n din favoritl\xE5t."
title: Refaktorisering
weight: 19
---

## Hur man gör:
Låt oss säga att du har ett stycke Haskell-kod som upprepar sig mer än din favoritlåt. Här är en snabb titt på hur du kan refaktorisera det med hjälp av funktioner.

Före refaktorisering:

```haskell
printInvoice :: String -> Float -> String -> IO ()
printInvoice kund totalt artikel = do
  putStrLn $ "Kund: " ++ kund
  putStrLn $ "Totalt: " ++ show totalt
  putStrLn $ "Artikel: " ++ artikel
```

Efter lite refaktorisering:

```haskell
printDetail :: String -> String -> IO ()
printDetail etikett värde = putStrLn $ etikett ++ ": " ++ värde

printInvoice :: String -> Float -> String -> IO ()
printInvoice kund totalt artikel = do
  printDetail "Kund" kund
  printDetail "Totalt" (show totalt)
  printDetail "Artikel" artikel

-- Exempel på utdata:
-- Kund: Alice
-- Totalt: $42.00
-- Artikel: Haskell Programmeringsguide
```

Som du kan se, genom att extrahera det gemensamma mönstret till en separat `printDetail`-funktion, undviker vi upprepning och gör `printInvoice` tydligare och lättare att hantera.

## Djupdykning
När Haskell dök upp i slutet av 80-talet var det tydligt att det funktionella paradigmet kunde bringa ny luft till kodningspraxis. Gå framåt i tiden, och refaktorisering i Haskell är särskilt elegant tack vare att funktioner är förstaklassmedborgare och dess starka statiska typsystem. Du refaktoriserar utan att frukta att du skulle bryta din app eftersom kompilatorn har din rygg.

Alternativ till manuell refaktorisering kan inkludera användning av automatiserade verktyg, även om den funktionella naturen och typsäkerheten hos Haskell ibland kan göra detta mindre förekommande jämfört med andra språk. När det gäller implementation är det viktigt att utnyttja Haskells funktioner såsom högre ordningens funktioner, renhet och oföränderlighet för att göra refaktoriseringen smidigare.

Refaktoriseringar som "Extrahera funktion", precis visad, är vanliga, men du kan också göra "Infoga funktion", "Byt namn på variabel" och "Ändra funktionsignatur" med förtroende tack vare typsystemet. Haskells kraftfulla typinferens kan ibland fånga fel som skulle glida igenom i andra språk.

## Se även
För en djupdykning i refaktorisering i Haskell, slå upp böckerna med "Refactoring: Improving the Design of Existing Code" av Martin Fowler, där koncepten är universellt tillämpliga. Kolla in hlint-verktyget för automatiska tips om att förbättra din Haskell-kod. Sväng också förbi Haskell wiki (https://wiki.haskell.org/Refactoring) för insikter från gemenskapen och ytterligare läsning.
