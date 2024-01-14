---
title:                "Haskell: Druck des Debug-Ausgangs"
simple_title:         "Druck des Debug-Ausgangs"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Debuggen ist ein wichtiger Teil des Entwicklungsprozesses für jeden Programmierer. Das Drucken von Debug-Ausgaben kann dabei helfen, Fehler und Probleme im Code zu finden und zu beheben. Es erlaubt uns auch, den Programmablauf zu verstehen und zu überprüfen, ob unsere Annahmen über die Daten und Variablen im Code korrekt sind. In Haskell gibt es verschiedene Möglichkeiten, Debug-Ausgaben zu drucken, und in diesem Blog-Post werden wir uns damit befassen, wie man dies effektiv tun kann.

## Wie geht das

Um Debug-Ausgaben in Haskell zu drucken, können wir die Funktion "putStrLn" aus dem "Prelude" Modul verwenden. Diese Funktion nimmt einen String als Argument und gibt ihn auf der Konsole aus. Zum Beispiel:

```Haskell
main = do
  putStrLn "Debug-Ausgabe"
```

Dieser Code würde einfach den String "Debug-Ausgabe" auf der Konsole ausgeben. Wir können auch Variablen oder andere Ausdrücke in den String einbinden, indem wir sie innerhalb von geschweiften Klammern platzieren und ein Dollarzeichen davor setzen, um sie auszuwerten. Zum Beispiel:

```Haskell
main = do
  let x = 10
  putStrLn $"Der Wert von x ist: {x}"
```

Dies würde den String "Der Wert von x ist: 10" ausgeben. Indem wir Variablenwerte oder Ausdrücke in diese Debug-Ausgaben einbinden, können wir sie effektiver nutzen, um den Programmablauf zu verfolgen und mögliche Fehler zu finden.

## Tiefergehende Informationen

Es gibt auch andere Funktionen und Techniken, um Debug-Ausgaben in Haskell zu drucken. Zum Beispiel können wir die Funktion "trace" aus dem Modul "Debug.Trace" verwenden. Diese Funktion funktioniert ähnlich wie "putStrLn", jedoch gibt sie den ausgewerteten Wert des Ausdrucks zurück, den wir angeben, anstatt ihn nur auszudrucken. Das kann uns dabei helfen, die Ausgabe von Debug-Anweisungen in unsere Programmlogik zu integrieren und Fehler zu finden, die möglicherweise nicht durch reine Ausgabe auf der Konsole gefunden werden können.

## Siehe auch

- [Die offizielle Dokumentation zu Debug-Ausgaben in Haskell](https://www.haskell.org/documentation.html)
- [Ein Tutorial zum Debuggen in Haskell](https://mmhaskell.com/debugging-in-haskell)
- [Ein Artikel über die Verwendung von "Debug.Trace" in Haskell](https://www.fpcomplete.com/blog/haskell/debugging-haskell-trace/)

Vielen Dank fürs Lesen und viel Erfolg beim Debuggen in Haskell!