---
title:                "Refactoring"
date:                  2024-01-28T22:06:02.188777-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/haskell/refactoring.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Refactoring is het proces van het aanpassen van je code zonder het externe gedrag ervan te veranderen. Het gaat helemaal om het opruimen en organiseren van je werk om de code gemakkelijker leesbaar, onderhoudbaar en uitbreidbaar te maken. Het kan ook helpen bij het oplossen van bugs en het verbeteren van de prestaties.

## Hoe:
Stel je hebt een stuk Haskell-code dat zich meer herhaalt dan je favoriete liedje. Hier is een snelle blik op hoe je dat zou kunnen refactoren met behulp van functies.

Voor refactoring:

```haskell
printInvoice :: String -> Float -> String -> IO ()
printInvoice klant totaal item = do
  putStrLn $ "Klant: " ++ klant
  putStrLn $ "Totaal: " ++ show totaal
  putStrLn $ "Item: " ++ item
```

Na een beetje refactoring:

```haskell
printDetail :: String -> String -> IO ()
printDetail label waarde = putStrLn $ label ++ ": " ++ waarde

printInvoice :: String -> Float -> String -> IO ()
printInvoice klant totaal item = do
  printDetail "Klant" klant
  printDetail "Totaal" (show totaal)
  printDetail "Item" item

-- Voorbeelduitvoer:
-- Klant: Alice
-- Totaal: $42.00
-- Item: Haskell Programmeringsgids
```

Zoals je kunt zien, door het gemeenschappelijke patroon in een aparte `printDetail` functie te extraheren, vermijden we herhaling en maken we `printInvoice` duidelijker en gemakkelijker te beheren.

## Diepe Duik
Toen Haskell eind jaren '80 op de scène verscheen, was het duidelijk dat het functionele paradigma wat frisse lucht kon brengen in de programmeerpraktijken. Fast forward, en refactoring in Haskell is bijzonder elegant dankzij functies die als first-class citizens worden beschouwd en zijn sterke statische type systeem. Je kunt refactoren zonder angst om je app te breken, aangezien de compiler je rug dekt.

Alternatieven voor handmatige refactoring kunnen het gebruik van geautomatiseerde hulpmiddelen omvatten, hoewel de functionele aard en typeveiligheid van Haskell dit soms minder prevalent kan maken in vergelijking met andere talen. Wat betreft de implementatie, is het belangrijk om Haskell's functies zoals hogere-orde functies, puurheid en onveranderlijkheid te benutten om refactoring soepeler te maken.

Refactorings zoals "Extract Function", net getoond, zijn gebruikelijk, maar je kunt ook "Inline Function", "Rename Variable" en "Change Function Signature" met vertrouwen doen, dankzij het type systeem. Haskell's krachtige type-inferentie kan soms fouten vangen die in andere talen door de mazen zouden glippen.

## Zie Ook
Voor een diepgaande duik in refactoring in Haskell, sla de boeken op met "Refactoring: Improving the Design of Existing Code" van Martin Fowler, waar de concepten universeel toepasbaar zijn. Bekijk de hlint-tool voor geautomatiseerde hints over het verbeteren van je Haskell-code. Bezoek ook de Haskell wiki (https://wiki.haskell.org/Refactoring) voor inzichten van de gemeenschap en verder lezen.
