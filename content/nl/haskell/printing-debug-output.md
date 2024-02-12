---
title:                "Debug-output afdrukken"
aliases:
- nl/haskell/printing-debug-output.md
date:                  2024-01-28T22:04:45.607781-07:00
model:                 gpt-4-0125-preview
simple_title:         "Debug-output afdrukken"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/haskell/printing-debug-output.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Debug-output afdrukken gaat over data uit je programma spugen om te zien wat er onder de motorkap gebeurt. Programmeurs doen dit om variabelen te volgen, de stroom te begrijpen en vervelende bugs plat te drukken.

## Hoe te:

Een eenvoudige manier om debug-informatie in Haskell af te drukken is met de `print` functie, die een waarde neemt die een instantie is van de `Show` typeclass en het uitvoert naar de console.

```Haskell
main :: IO ()
main = do
  let number = 42
  print number
  putStrLn "Debuggen is een fluitje van een cent in Haskell!"

-- Uitvoer:
-- 42
-- Debuggen is een fluitje van een cent in Haskell!
```

Voor meer complexe datastructuren, zorg ervoor dat ze `Show` afleiden om mooi afdrukken mogelijk te maken:

```Haskell
data Cake = Chocolate | Vanilla deriving Show

debugSmaak :: Cake -> IO ()
debugSmaak smaak = print smaak

main :: IO ()
main = debugSmaak Chocolate

-- Uitvoer:
-- Chocolate
```

Soms willen we tijdelijke debugging die later makkelijk te verwijderen is. Voer de `Debug.Trace` module in.

```Haskell
import Debug.Trace (trace)

main :: IO ()
main = putStrLn $ trace "Dit wordt eerst afgedrukt" "Dit wordt als tweede afgedrukt"

-- Uitvoer:
-- Dit wordt eerst afgedrukt
-- Dit wordt als tweede afgedrukt
```

De `trace` functie drukt de string af wanneer de waarde wordt geëvalueerd, maar het is een bijeffect in het pure deel van de code. Het is handig, maar gebruik met voorzichtigheid!

## Diepgaand

In de goede oude tijd, zou debugging misschien de oude truc van "print statement" zijn geweest. Haskell biedt dit met een functionele draai en tools voor schonere debug-praktijken. Voer `print` en de `Debug.Trace` module in, zoals eerder verkend.

Alternatieven voor `print` zijn onder andere `putStrLn` voor strings en `putStr`, als je niet van die automatische nieuwe regel houdt. `Debug.Trace` heeft ook varianten zoals `traceShow` die direct met `Show` instanties werken, waardoor je een `show` aanroep bespaart.

Wat betreft implementatiedetails, `print` is in principe `putStrLn . show`. Het drukt alle `Show`-bare data af naar stdout. `Debug.Trace` functies, aan de andere kant, zijn bedoeld voor tijdelijk gebruik tijdens de ontwikkeling. Ze sluipen in pure code en schenden referentiële transparantie, wat op de lange termijn een no-no is.

Vergeet niet de logboekbibliotheken voor serieuze applicaties, die meer controle bieden en minder "debug by print."

## Zie Ook

- De `Debug.Trace` documentatie: [https://hackage.haskell.org/package/base/docs/Debug-Trace.html](https://hackage.haskell.org/package/base/docs/Debug-Trace.html)
- Haskell Wiki over Debugging: [https://wiki.haskell.org/Debugging](https://wiki.haskell.org/Debugging)
- Een boeiende discussie over waarom `Debug.Trace` niet gebruiken en wat in plaats daarvan te doen: [https://stackoverflow.com/questions/7741400/why-is-using-debug-trace-considered-bad-practice](https://stackoverflow.com/questions/7741400/why-is-using-debug-trace-considered-bad-practice)
