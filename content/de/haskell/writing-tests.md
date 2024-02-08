---
title:                "Tests Schreiben"
aliases:
- de/haskell/writing-tests.md
date:                  2024-02-03T19:30:37.210856-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tests Schreiben"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Tests in Haskell zu schreiben, dient dazu, sicherzustellen, dass Ihre Funktionen wie erwartet durch automatisierte Überprüfungen arbeiten. Entwickler tun dies, um frühzeitig Fehler zu finden, das Refactoring zu erleichtern und das Verhalten zu dokumentieren, was die Codebasis wartbarer und skalierbarer macht.

## Wie:

Haskell unterstützt verschiedene Test-Frameworks, aber zwei beliebte sind `Hspec` und `QuickCheck`. Hspec ermöglicht es Ihnen, menschenlesbare Spezifikationen für Ihren Code zu definieren, während QuickCheck es Ihnen erlaubt, Tests automatisch zu generieren, indem Sie Eigenschaften beschreiben, die Ihr Code erfüllen sollte.

### Hspec verwenden

Fügen Sie zuerst `hspec` zu Ihrer Build-Tool-Konfiguration hinzu (z.B. `stack.yaml` oder `cabal`-Datei). Importieren Sie dann `Test.Hspec` und schreiben Sie Tests als Spezifikationen:

```haskell
-- Datei: spec/MyLibSpec.hs
import Test.Hspec
import MyLib (add)

main :: IO ()
main = hspec $ describe "MyLib.add" $ do
  it "addiert zwei Zahlen" $
    add 1 2 `shouldBe` 3

  it "gibt die erste Zahl zurück, wenn Null addiert wird" $
    add 5 0 `shouldBe` 5
```

Führen Sie dann Ihre Tests mit Ihrem Build-Tool aus, was zu einer Ausgabe führen könnte, die so aussieht:

```
MyLib.add
  - addiert zwei Zahlen
  - gibt die erste Zahl zurück, wenn Null addiert wird

Beendet in 0.0001 Sekunden
2 Beispiele, 0 Fehler
```

### QuickCheck verwenden

Mit QuickCheck drücken Sie Eigenschaften aus, die Ihre Funktionen erfüllen sollten. Fügen Sie `QuickCheck` zu Ihrer Projektkonfiguration hinzu und importieren Sie es:

```haskell
-- Datei: test/MyLibProperties.hs
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

Das Ausführen dieser Tests generiert automatisch Eingaben, um die angegebenen Eigenschaften zu überprüfen:

```
+++ OK, bestanden 100 Tests.
+++ OK, bestanden 100 Tests.
```

In beiden Beispielen, Hspec und QuickCheck, dienen die Test-Suiten als ausführbare Dokumentation, die die Korrektheit Ihres Codes automatisch überprüfen kann.
