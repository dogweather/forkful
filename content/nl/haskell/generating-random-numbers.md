---
title:                "Willekeurige getallen genereren"
date:                  2024-01-28T22:01:06.560271-07:00
model:                 gpt-4-0125-preview
simple_title:         "Willekeurige getallen genereren"

category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/haskell/generating-random-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het genereren van willekeurige getallen in Haskell houdt in dat er getallen worden gecreëerd die onvoorspelbaar zijn volgens menselijke normen. Dit is cruciaal in scenario's variërend van cryptografische toepassingen tot simulaties waarbij het element van kans nodig is om real-world fenomenen nauwkeurig te modelleren.

## Hoe:

Om willekeurige getallen te genereren in Haskell, gebruikt men typisch het `random` pakket, dat deel uitmaakt van het Haskell Platform. Hier is een stap-voor-stap handleiding:

Zorg eerst dat je het `random` pakket geïnstalleerd hebt. Zo niet, dan kun je het via Cabal of Stack krijgen.

### Een Willekeurig Getal Genereren

Om een eenvoudig willekeurig getal te genereren, kun je de `randomRIO` functie gebruiken, die een willekeurige waarde produceert binnen een opgegeven reikwijdte.

```Haskell
import System.Random (randomRIO)

main :: IO ()
main = do
  randomNumber <- randomRIO (1, 10) :: IO Int
  putStrLn $ "Willekeurig getal: " ++ show randomNumber
```

### Een Lijst van Willekeurige Getallen Genereren

Het genereren van een lijst van willekeurige getallen is iets complexer maar nog steeds eenvoudig:

```Haskell
import System.Random (randomRIO)

randomList :: Int -> IO [Int]
randomList 0 = return []
randomList n = do
  r <- randomRIO (1, 100)
  rs <- randomList (n-1)
  return (r:rs)

main :: IO ()
main = do
  getallen <- randomList 5
  print getallen
```

Dit codefragment creëert een functie `randomList` die een lijst van willekeurige gehele getallen genereert. Vervang `(1, 100)` met je gewenste bereik.

## Diepgaand Onderzoek

Het Haskell `random` pakket biedt een pseudo-willekeurige getallengenerator (PRNG), wat betekent dat de gegenereerde getallen niet echt willekeurig zijn maar wel willekeurig kunnen lijken voor veel toepassingen. De kern van Haskell's vermogen om willekeurige getallen te genereren ligt in de `RandomGen` typeklasse, die verschillende methoden van willekeurige getalgeneratie abstraheert, en de `Random` typeklasse, die typen bevat die willekeurig gegenereerd kunnen worden.

Historisch gezien heeft de benadering van Haskell tot willekeurige getalgeneratie de nadruk gelegd op zuiverheid en reproduceerbaarheid. Dit is waarom operaties die willekeurigheid betreffen expliciet worden afgehandeld in de `IO` monade of vereisen dat generatorstaten handmatig worden doorgegeven en bijgewerkt — om referentiële transparantie te handhaven.

In bepaalde toepassingen, zoals cryptografie, zijn de pseudo-willekeurige getallen die door de standaard PRNG worden gegenereerd mogelijk niet veilig genoeg. Voor deze use cases wenden Haskell-programmeurs zich vaak tot meer gespecialiseerde bibliotheken zoals `crypto-random`, die ontworpen zijn om te voldoen aan de strenge eisen van cryptografische toepassingen.

Bovendien bieden alternatieve bibliotheken zoals `mwc-random` betere prestaties en kwaliteit van willekeurige getallen voor simulaties en andere toepassingen, door moderne algoritmen zoals de Mersenne Twister te implementeren.

Wanneer je een benadering voor het genereren van willekeurige getallen in Haskell kiest, is het essentieel om de behoeften van de toepassing met betrekking tot kwaliteit van willekeur, prestaties en veiligheid te overwegen om de meest geschikte tool of bibliotheek te selecteren.
