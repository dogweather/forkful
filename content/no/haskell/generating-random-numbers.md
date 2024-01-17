---
title:                "Generering av tilfeldige tall"
html_title:           "Haskell: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Generering av tilfeldige tall er en viktig del av programmering, spesielt i spillet industrien. Det er en måte for programmerere å lage tilfeldighet og variasjon i sine programmer. Ved å bruke tilfeldige tall, kan programmene bli mer realistiske og interessante for brukerne.

# Hvordan å:
Her er et enkelt eksempel på hvordan du genererer tilfeldige tall i Haskell:

```Haskell
--Importerer pakken for tilfeldige tall
import System.Random

--Funksjon for å generere et tilfeldig tall mellom 1 og 10
tilfeldigTall :: IO Int
tilfeldigTall = randomRIO (1,10)

--Kaller funksjonen for å generere et tilfeldig tall
main = do
  tall <- tilfeldigTall
  print tall
```

Eksempel på output:

```Haskell
8
```

Generering av tilfeldige tall kan også kombineres med andre funksjoner, for eksempel å fylle en liste med tilfeldige tall:

```Haskell
--Funksjon for å generere en liste med tilfeldige tall mellom 1 og 100
tilfeldigListe :: IO [Int]
tilfeldigListe = sequence $ replicate 10 (randomRIO (1,100))

--Kaller funksjonen for å generere en liste med tilfeldige tall
main = do
  liste <- tilfeldigListe
  print liste
```

Eksempel på output:

```Haskell
[64, 8, 91, 27, 57, 36, 2, 99, 73, 40]
```

# Dypdykk:
Tilfeldige tall ble en viktig del av programmering på 1950-tallet, da spill og simuleringer ble utviklet. Før dette måtte tilfeldige tall bli generert manuelt ved hjelp av fysiske metoder som å riste terninger eller bruke kortstokker. I dag brukes tilfeldige tall ikke bare i spill, men også i kryptografi og statistisk analyse av data.

Det finnes ulike algoritmer for å generere tilfeldige tall, hvor noen er mer optimalisert for tilfeldighet enn andre. Haskell bruker en algoritme kalt "Mersenne Twister" som er en av de mest vanlige og pålitelige metodene for å generere tilfeldige tall.

# Se Også:
Hvis du vil lære mer om generering av tilfeldige tall i Haskell, kan du se på dokumentasjonen for "System.Random" pakken: https://hackage.haskell.org/package/random

Du kan også lese om de ulike tilfeldige tall algoritmene og deres effektivitet her: https://en.wikipedia.org/wiki/List_of_random_number_generators