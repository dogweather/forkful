---
title:    "Elm: Debug-Ausgabe drucken"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Das Ausgeben von Debug-Ausgaben ist ein wichtiger Teil der Programmierung, vor allem in einer funktionalen Sprache wie Elm. Mit der Möglichkeit, Code-Blöcke und spezifische Variablen auszugeben, ist es ein nützliches Werkzeug zur Fehlerbehebung und zum Verständnis des Programmablaufs.

## Wie man es macht

Die Syntax zum Ausgeben von Debug-Ausgaben in Elm ist relativ einfach. In einer beliebigen Funktion, in der Sie Debug-Ausgaben sehen möchten, müssen Sie ```debug``` mit dem zu überprüfenden Wert kombinieren. Zum Beispiel: 

```Elm
someFunction : Int -> String
someFunction number =
    debug (toString number) ++ " ist eine Zahl"
```

Dieser Code wird die Zahl in eine Zeichenkette konvertieren und dann mit dem Text " ist eine Zahl" verbinden. Die Ausgabe sieht folgendermaßen aus:

```
1 ist eine Zahl
```

Sie können auch komplexere Debug-Ausgaben erstellen, indem Sie ganze Ausdrücke verwenden, z.B. ```(toString (number * 2))```. Wenn Sie mehrere Debug-Ausgaben in einer Funktion haben, wird jede Ausgabe in einer separaten Zeile angezeigt.

## Tiefere Einblicke

Es gibt auch einige nützliche Funktionen im Modul ```Debug``` mit denen Sie noch spezifischere Informationen ausgeben können. Zum Beispiel können Sie mit ```watch``` bestimmte Variablen während des Programmablaufs überwachen, oder mit ```crash``` eine Fehlermeldung ausgeben und dadurch den Programmablauf unterbrechen. Sie können auch eine Liste von Werten mit ```list``` ausgeben, um die Elemente einer Datenstruktur zu überprüfen.

Es ist auch wichtig zu beachten, dass Debug-Ausgaben in einer Produktionsumgebung nicht angezeigt werden, daher müssen Sie sich keine Sorgen machen, dass sie versehentlich vom Benutzer gesehen werden.

## Siehe auch

- [Elm Debug Module](https://package.elm-lang.org/packages/elm/core/latest/Debug)
- [Debugging in Elm (auf Englisch)](https://www.elm-tutorial.org/en/04-debugging/00-introduction.html)