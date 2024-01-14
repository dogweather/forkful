---
title:    "Haskell: Verwendung regulärer Ausdrücke"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Warum

Regular Expressions sind ein leistungsstarkes Werkzeug, das dir dabei helfen kann, Zeichenketten in deinen Haskell Programmen zu verarbeiten. Sie ermöglichen es dir, komplexe Muster in Texten zu finden, zu ersetzen oder zu validieren. Mit Regular Expressions kannst du deine Programmierung auf ein neues Level bringen und effizienter arbeiten.

## Wie es geht

Um reguläre Ausdrücke in Haskell zu verwenden, musst du das `regex-base` Paket importieren. Schauen wir uns an, wie man einen einfachen regulären Ausdruck verwendet, um alle Wörter in einem Text zu finden.

```Haskell
import Text.Regex.Base
import Text.Regex.Posix

str = "Hallo, mein Name ist Max."

-- Finde alle Wörter in str
words = getAllTextMatches (str =~ "\\w+")
```
Die Ausgabe wird so aussehen: `["Hallo", "mein", "Name", "ist", "Max"]`. Hier haben wir den regulären Ausdruck `\\w+` verwendet, der für ein oder mehrere alphanumerische Zeichen steht.

Du kannst auch reguläre Ausdrücke verwenden, um Text zu ersetzen oder zu validieren. Schauen wir uns ein Beispiel an, wie man alle Kommas in einem Text durch Punkte ersetzen kann.

```Haskell
str = "Das ist ein Beispiel, wie man Text ersetzen kann."

-- Ersetze alle Kommas durch Punkte
newStr = subRegex (mkRegex ",") str "."
```
Die Ausgabe wird so aussehen: `"Das ist ein Beispiel. wie man Text ersetzen kann."`. Hier haben wir `mkRegex` verwendet, um eine Regex-Instanz zu erstellen und `subRegex` um die Ersetzung durchzuführen.

## Tiefentauchen

Regular Expressions unterstützen viele nützliche Funktionen wie Backreferences, Anker oder Quantoren. Diese werden verwendet, um komplexe Muster zu erstellen oder spezifische Teile des Textes zu finden. Es lohnt sich, sich mit diesen Funktionen auseinanderzusetzen, um das volle Potential von regulären Ausdrücken auszuschöpfen.

Eine gute Ressource zum Lernen und Üben von regulären Ausdrücken in Haskell ist das `regex-tutor` Paket. Es bietet interaktive Übungen und Beispiele, die dir dabei helfen, deine Fähigkeiten zu verbessern.

## Siehe auch

- [Das regex-base Paket](https://hackage.haskell.org/package/regex-base)
- [Die offizielle Dokumentation zu regulären Ausdrücken in Haskell](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/regex-base-0.94.0.0/Text-Regex-Base.html)
- [Das regex-tutor Paket](https://hackage.haskell.org/package/regex-tutor)