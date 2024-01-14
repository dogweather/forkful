---
title:    "Elm: Suchen und Ersetzen von Text"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Warum
In der Welt des Programmierens ist es oft wichtig, Text zu durchsuchen und zu ersetzen, sei es in Skripten, Konfigurationsdateien oder Quellcode. Dies kann sowohl zeitsparend als auch fehlervermeidend sein und ist daher ein wichtiger Schritt im Entwicklungsprozess.

## Anleitung
Um Text in Elm zu suchen und zu ersetzen, können wir die Funktion `replace` aus dem Paket `String` verwenden. Hier ist ein Beispiel, wie wir alle Vorkommen von "Hund" in einer Zeichenkette durch "Katze" ersetzen können:

```Elm
import String exposing (replace)

satz = "Ich habe einen Hund, er ist sehr süß."

newSatz = replace "Hund" "Katze" satz

-- newSatz wird zu "Ich habe eine Katze, sie ist sehr süß."
```

Wir können auch mit regulären Ausdrücken arbeiten, indem wir das `Regex` Paket verwenden. Hier ist ein Beispiel, wie wir alle Vorkommen von Zahlen in einer Zeichenkette entfernen können:

```Elm
import Regex exposing (replace, regex)

satz = "Es gibt 8 Milliarden Menschen auf der Welt."

newSatz = replace (regex "\\d") (\_ -> "") satz

-- newSatz wird zu "Es gibt Milliarden Menschen auf der Welt."
```

Diese Methoden können auf verschiedene Arten kombiniert werden, um komplexe Such- und Ersetzungsvorgänge durchzuführen.

## Tiefere Einblicke
Es ist wichtig zu beachten, dass bei der Verwendung von `replace` oder `Regex.replace` immer eine neue Zeichenkette zurückgegeben wird. Das heißt, wir müssen die neue, veränderte Zeichenkette einer Variablen zuweisen, um sie verwenden zu können.

Außerdem unterstützt `replace` die Option `All` für die Parameter `flag` und `count`, was bedeutet, dass alle Vorkommen des Suchmusters ersetzt werden. Standardmäßig wird nur das erste Vorkommen ersetzt. Diese Option ist besonders nützlich, wenn wir alle Vorkommen eines bestimmten Wortes oder Zeichens in einem Text ersetzen möchten.

## Siehe auch
- [Dokumentation von String.replace](https://package.elm-lang.org/packages/elm/core/latest/String#replace)
- [Dokumentation von Regex.replace](https://package.elm-lang.org/packages/elm/regex/latest/Regex#replace)
- [Beispiele für reguläre Ausdrücke in Elm](https://devhints.io/elm-regex)