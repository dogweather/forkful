---
title:    "Haskell: Ausgabe von Debugging-Informationen drucken"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Eine der wichtigsten Fähigkeiten beim Programmieren ist es, die eigene Codebasis zu verstehen und zu beheben, wenn es zu Fehlern kommt. Eine nützliche Methode, um dieses Verständnis zu verbessern, ist das Drucken von Debug-Ausgaben. In diesem Blogbeitrag werden wir uns ansehen, warum es sinnvoll ist, Debug-Ausgaben in Haskell zu verwenden und wie man sie umsetzt.

## Wie geht das

Das Drucken von Debug-Ausgaben in Haskell ist einfach und kann auf verschiedene Arten erfolgen. Eine Möglichkeit ist die Verwendung der Funktion `trace` aus dem `Debug.Trace`-Modul. Diese Funktion akzeptiert einen beliebigen Datentyp als Eingabe und gibt diesen zusammen mit einer Debug-Nachricht auf der Konsole aus.

```Haskell
import Debug.Trace (trace)

foo :: Int -> Int
foo x = trace ("Die Funktion wurde mit dem Wert " ++ show x ++ " aufgerufen.") x + 1

main = print (foo 5)
```

Die Ausgabe dieses Codes wäre:

```
Die Funktion wurde mit dem Wert 5 aufgerufen.
6
```

Eine andere Möglichkeit ist die Verwendung der Funktion `putStrLn` aus dem `System.IO`-Modul. Diese Funktion gibt eine Nachricht auf der Konsole aus, die übergebenen Werte werden automatisch in Strings konvertiert.

```Haskell
import System.IO (putStrLn)

foo :: Int -> Int
foo x = x + 1

main = do
  putStrLn "Start der Funktion."
  print (foo 5)
  putStrLn "Ende der Funktion."
```

Die Ausgabe wäre hier:

```
Start der Funktion.
6
Ende der Funktion.
```

Beide Methoden sind nützlich, um den Codefluss besser zu verstehen und Fehler zu identifizieren.

## Tieferer Einblick

Neben dem einfachen Drucken von Werten gibt es noch weitere Möglichkeiten, Debug-Ausgaben in Haskell zu verwenden. Eine Möglichkeit ist das Hinzufügen von Bedingungen für das Drucken von Ausgaben, um nur in bestimmten Situationen die Debug-Ausgaben zu erhalten.

```Haskell
import Debug.Trace (trace)

foo :: Int -> Int
foo x =
  trace (if x > 10 then "Der Wert ist größer als 10" else "Der Wert ist kleiner oder gleich 10") x + 1

main = print (foo 5)
```

In diesem Beispiel wird nur dann die Debug-Ausgabe ausgegeben, wenn der Wert größer als 10 ist. Dies ist nützlich, um nur die wichtigsten Informationen zu erhalten.

Eine weitere Möglichkeit ist die Verwendung von `Debug.Trace.traceShow` oder `Debug.Trace.traceShowId` aus dem `Debug.Trace`-Modul. Diese Funktionen ermöglichen die Ausgabe von komplexen Datenstrukturen und das Verständnis des Codeflusses anhand dieser Ausgaben.

```Haskell
import Debug.Trace (traceShowId)

foo :: [Int] -> [Int]
foo xs = traceShowId xs

main = do
  let list = [1, 2, 3]
  print (foo list)
```

Die Ausgabe wäre hier:

```
[1,2,3]
```

Wie man sehen kann, gibt die Funktion `traceShowId` den übergebenen Wert als Ausgabe zurück und ermöglicht somit das Drucken von komplexen Datenstrukturen ohne zusätzlichen Code.

## Siehe auch

- [Debug-Ausgaben in Haskell verwenden (Englisch)](https://www.realworldhaskell.org/Chapter_7:_Debugging,_testing,_and_profiling)
- [Einfaches Debugging in Haskell (Englisch)](https://www.fpcomplete.com/blog/2017/09/easy-debugging-with-trace)