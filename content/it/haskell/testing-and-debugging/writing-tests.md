---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:46.390929-07:00
description: "Scrivere test in Haskell consiste nell'assicurarsi che le tue funzioni\
  \ funzionino come previsto attraverso controlli automatizzati. I programmatori lo\u2026"
lastmod: '2024-03-13T22:44:43.478719-06:00'
model: gpt-4-0125-preview
summary: Scrivere test in Haskell consiste nell'assicurarsi che le tue funzioni funzionino
  come previsto attraverso controlli automatizzati.
title: Scrivere test
weight: 36
---

## Cosa & Perché?

Scrivere test in Haskell consiste nell'assicurarsi che le tue funzioni funzionino come previsto attraverso controlli automatizzati. I programmatori lo fanno per individuare precocemente i bug, facilitare il refactoring e documentare il comportamento, rendendo il codice più manutenibile e scalabile.

## Come fare:

Haskell supporta vari framework per i test, ma due popolari sono `Hspec` e `QuickCheck`. Hspec ti permette di definire specifiche leggibili per il tuo codice, mentre QuickCheck ti permette di generare automaticamente test descrivendo proprietà che il tuo codice dovrebbe soddisfare.

### Usare Hspec

Prima, aggiungi `hspec` alla configurazione del tuo strumento di costruzione (ad es., `stack.yaml` o file `cabal`). Poi, importa `Test.Hspec` e scrivi i test come specifiche:

```haskell
-- file: spec/MyLibSpec.hs
import Test.Hspec
import MyLib (add)

main :: IO ()
main = hspec $ describe "MyLib.add" $ do
  it "aggiunge due numeri" $
    add 1 2 `shouldBe` 3

  it "restituisce il primo numero quando si aggiunge zero" $
    add 5 0 `shouldBe` 5
```

Poi, esegui i tuoi test usando il tuo strumento di costruzione, ottenendo un output che potrebbe assomigliare a:

```
MyLib.add
  - aggiunge due numeri
  - restituisce il primo numero quando si aggiunge zero

Terminato in 0.0001 secondi
2 esempi, 0 fallimenti
```

### Usare QuickCheck

Con QuickCheck, esprimi proprietà che le tue funzioni dovrebbero soddisfare. Aggiungi `QuickCheck` alla configurazione del tuo progetto, poi importalo:

```haskell
-- file: test/MyLibProperties.hs
import Test.QuickCheck
import MyLib (add)

prop_addAssociative :: Int -> Int -> Int -> Bool
prop_addAssociative x y z = x + (y + z) == (x + y) + z

prop_addCommutative :: Int -> Int -> Bool
prop_addCommutative x y = x + y == y + x

main :: IO ()
main = do
  quickCheck prop_addAssociative
  quickCheck prop_addCommutative
```

Eseguendo questi test verranno generati automaticamente input per verificare le proprietà specificate:

```
+++ OK, superato 100 test.
+++ OK, superato 100 test.
```

In entrambi gli esempi di Hspec e QuickCheck, le suite di test fungono da documentazione eseguibile che può verificare automaticamente la correttezza del tuo codice.
