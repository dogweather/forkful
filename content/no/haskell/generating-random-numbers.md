---
title:                "Generere tilfeldige tall"
html_title:           "Arduino: Generere tilfeldige tall"
simple_title:         "Generere tilfeldige tall"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

---
title: Generering av tilfeldige tall i Haskell
---

## Hva & Hvorfor?
Tilfeldige tall generering er prosessen med å lage tall som ikke kan forutsies bedre enn ved tilfeldig sjanse. Dette brukes ofte i programmering for alt fra simuleringer til spill, kryptografi, og systemtestinger.

## Hvordan:
Ved å bruke biblioteket `System.Random`, kan du generere tilfeldige tall i Haskell. Her er et enkelt eksempel på generering av et tilfeldig heltall:

```Haskell
import System.Random
main = do 
    randomNumber <- randomRIO (1,100) :: IO Int
    print(randomNumber)
```
Når dette programmet kjøres, vil det skrive ut et tilfeldig tall mellom 1 og 100. 

## Dyp Dykk
Historisk sett, har generering av tilfeldige tall vært en utfordring i datamaskinverdenen. Til å begynne med var datamaskiner designet for å være fullstendig forutsigbare og ettergivende. Tilfeldighet var antittese av dette. Forskjellige metoder har blitt utviklet for å gi en kilde til tilfeldighet i disse systemene. 

Å bruke funksjonen `randomRIO` er ikke det eneste alternativet i Haskell. Det finnes også andre funksjoner i `System.Random`-biblioteket som `randomIO`, `randomR` og `random`.

Detaljer om implementasjonen er ikke utstilt i dette eksemplet, men det er verdt å merke seg at Haskell bruker en pseudo-tilfeldig tallgenerator. Det betyr at de genererte tallene bare er tilfeldige til en viss grad, og en tilstrekkelig smart angriper potensielt kunne forutsi dem.

## Se Også
- For mer informasjon om `System.Random` biblioteket, sjekk ut [Haskell Documentation](https://hackage.haskell.org/package/random-1.1/docs/System-Random.html)
- Du kan lære mer om generering av tilfeldige tall i datamaskiner fra [Wikipedia](https://en.wikipedia.org/wiki/Random_number_generation) 
- For alternativer til `System.Random`, se på [RandomGen](https://hackage.haskell.org/package/random-1.1/docs/System-Random.html#g:4) som gir en måte å produsere din egen strøm av tilfeldige tall.