---
date: 2024-01-20 17:47:26.605605-07:00
description: "How to: In Haskell, ist die Funktion `length` ein Teil der Prelude und\
  \ z\xE4hlt die Elemente einer Liste. Da Strings in Haskell Listen von Charakteren\
  \ sind,\u2026"
lastmod: '2024-04-05T21:53:55.806140-06:00'
model: gpt-4-1106-preview
summary: "In Haskell, ist die Funktion `length` ein Teil der Prelude und z\xE4hlt\
  \ die Elemente einer Liste."
title: "Ermittlung der Zeichenkettenl\xE4nge"
weight: 7
---

## How to:
```Haskell
main :: IO ()
main = do
    let text = "Hallo Welt"
    print $ length text -- Gibt die Länge des Strings aus
```

Ausgabe:
```
10
```

## Deep Dive
In Haskell, ist die Funktion `length` ein Teil der Prelude und zählt die Elemente einer Liste. Da Strings in Haskell Listen von Charakteren sind, zählt `length` einfach die Charaktere. Das ist einfach und elegant, aber nicht immer effizient bei langen Strings oder wenn nur geprüft werden soll, ob die Länge einen bestimmten Wert überschreitet.

Historisch gesehen hat Haskell immer versucht, Operationen so generisch wie möglich zu gestalten. `length` ist da keine Ausnahme. Es funktioniert nicht nur für Strings, sondern für alle Listen.

Alternativen zu `length` könnten beispielsweise Lazy Evaluation besser ausnutzen, wie `Data.Text.length` aus dem `text` Paket, das speziell für Textoperationen entworfen wurde und oft eine bessere Performance als die Standard-String-Implementierung bietet.

In Bezug auf Implementation, `length` ist eigentlich durch die Funktion `foldr` realisiert, die über die Liste iteriert und dabei ein akkumulatives Ergebnis aufbaut – in diesem Fall die Zählung der Elemente.

## See Also
- Die Haskell Prelude Dokumentation: [https://hackage.haskell.org/package/base/docs/Prelude.html](https://hackage.haskell.org/package/base/docs/Prelude.html)
- Das `text` Paket für effizientere String-Operationen: [https://hackage.haskell.org/package/text](https://hackage.haskell.org/package/text)
