---
title:                "Generowanie liczb losowych"
date:                  2024-01-20T17:49:19.374748-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generowanie liczb losowych"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Generowanie losowych liczb to po prostu wymyślanie numerków, jak z czarodziejskiej kapelusza. Programiści robią to, żeby dodać nieprzewidywalnośći – od prostych rzeczy jak losowanie czy też w grach komputerowych, po zaawansowane symulacje i badania naukowe.

## Jak to zrobić:

```Elm
import Random

-- Inicjalizacja generatora - potrzebujemy ziarna (seed).
initialSeed : Random.Seed
initialSeed = Random.initialSeed 42  -- "42" to przykładowa wartość ziarna.

-- Generator pojedynczej losowej liczby.
generateRandomNumber : Random.Generator Int
generateRandomNumber = Random.int 0 100  -- Losuje liczby od 0 do 100.

-- Przykład użycia generatora.
getRandomNumber : Random.Seed -> ( Int, Random.Seed )
getRandomNumber seed =
    Random.step generateRandomNumber seed

-- Wywołujemy generator z naszym seedem.
(result, newSeed) = getRandomNumber initialSeed

-- Wynik: Zobaczysz liczbę od 0 do 100 i nowe ziarno.
```
Właściwe wywołanie generatora wymaga nowego ziarna za każdym razem, by zapewnić różne wyniki.

## Deep Dive

Generowanie losowych liczb w Elm nie zawsze było takie proste. Wcześniej używano bibliotek jak `elm-random`, ale od momentu wprowadzenia Elm 0.17, wszystko idzie przez `Random` z wbudowaną kontrolą stanów ziaren. Co ważne, Elm traktuje losowość jako efekt stronny - dlatego potrzeba "ziarna" i nowych wartości seed dla kolejnych losowań, żeby wciąż było "funkcyjnie czyste". Alternatywy jak JavaScript `Math.random()` są używane kiedy chcemy coś poza Eml, ale pamiętajmy, że to łamie czystość funkcji. W implementacji szczególna uwaga jest na algorytmy generujące liczby pseudolosowe, co jest standardem w programowaniu funkcjonalnym.

## Zobacz Również

- [Oficjalna dokumentacja Elm o module Random](https://package.elm-lang.org/packages/elm/random/latest/)
- [Elm Guide on Effects](https://guide.elm-lang.org/effects/random.html) - Jak efekty, w tym losowe, działają w Elm i jak ich używać.
- [Wikipedia o pseudolosowości](https://pl.wikipedia.org/wiki/Generator_liczb_pseudolosowych) - Kontekst historyczny i teoretyczny na temat generowania liczb pseudolosowych.
