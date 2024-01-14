---
title:    "Elm: Text suchen und ersetzen"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Textsuche und -ersetzung ist ein wichtiger Teil des Programmierens, insbesondere wenn man mit größeren Mengen an Code arbeitet. Mit der richtigen Technik kann man effizient und schnell Änderungen in verschiedenen Dateien vornehmen und so Zeit und Mühe sparen.

## Wie geht es

Die Grundlage für die Textsuche und -ersetzung in Elm ist die Funktion `String.replace` aus dem Modul `String`. Sie nimmt als Argumente das zu ersetzende Muster und den Text, in dem es ersetzt werden soll, und gibt den Text mit dem entsprechenden Ersatz zurück.

```Elm
import String  -- Modul für die Textverarbeitung

text = "Heute ist ein schöner Tag"
ersetzt = String.replace "schöner" "hässlicher" text
-- gibt "Heute ist ein hässlicher Tag" zurück
```

Zusätzlich können reguläre Ausdrücke verwendet werden, um komplexere Muster zu erkennen und zu ersetzen. Das `Regex`-Modul bietet hierfür die Funktion `replace` an.

```Elm
import Regex  -- Modul für reguläre Ausdrücke

text = "123abc456def"
pattern =Regex.regex "[a-z]+"
ersatz = "xyz"
ersetzt = Regex.replace pattern ersatz text
-- gibt "123xyz456xyz" zurück
```

## Tiefer Einblick

Ein interessantes Feature beim Ersetzen von Text in Elm ist die Möglichkeit, eine Funktion als Ersatz zu verwenden. Diese Funktion nimmt das gefundene Muster als Argument und gibt den entsprechenden Ersatz zurück. Dadurch können dynamische Ersetzungen durchgeführt werden.

```Elm
text = "20$ sind ein fairer Preis"
pattern = Regex.regex "[0-9]+" -- findet alle Zahlen im Text
ersatzFunc = (\match -> case match of
                           "20" -> "10"  -- ersetzt 20 mit 10
                           "fairer" -> "unfairer"  -- ersetzt fairer mit unfairer
                           _ -> match)  -- lässt alles andere unverändert
ersetzt = Regex.replace pattern ersatzFunc text
-- gibt "10$ sind ein unfairer Preis" zurück
```

Es ist auch möglich, die `replace`-Funktion auf Teilstrings zu beschränken, indem man ein zusätzliches Argument für die maximale Anzahl an Ersetzungen übergibt.

```Elm
text = "Wiederholung Wiederholung Wiederholung"
pattern = Regex.regex "Wiederholung"
ersatz = "Repetition"
-- hier wird nur die erste Wiederholung ersetzt
ersetzt = Regex.replace pattern ersatz text 1
-- gibt "Repetition Wiederholung Wiederholung" zurück
```

Damit sind bereits einige Möglichkeiten für die Textsuche und -ersetzung in Elm abgedeckt. Weitere Informationen und Beispiele können in der [offiziellen Dokumentation](https://package.elm-lang.org/packages/elm/regex/latest/) und im [Regex-Beispielprojekt](https://github.com/elm/projects/blob/master/examples/Regex.elm) gefunden werden.

## Siehe auch

- [Elm Text Encoding](https://elm-lang.org/docs/utf8)
- [Pattern Matching in Elm](https://medium.com/@cscalfani/pattern-matching-in-elm-92adcbbb0ba5)
- [Elm Packages](https://package.elm-lang.org/)