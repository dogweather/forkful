---
title:                "Haskell: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum
Das Schreiben von Fehlern in die Standardfehlerausgabe ist ein wichtiges Konzept beim Programmieren in Haskell. Es gibt uns die Möglichkeit, Fehlermeldungen und Warnungen zu schreiben, die bei der Ausführung des Programms auf der Konsole angezeigt werden. Dadurch können wir potenzielle Probleme im Code schnell erkennen und beheben.

## Wie man es tut
Um eine Nachricht in die Standardfehlerausgabe zu schreiben, können wir die Funktion `hPutStrLn` aus dem `System.IO`-Modul verwenden. Diese Funktion nimmt als erstes Argument einen `Handle`, der die Ausgabedatei darstellt, und als zweites Argument die Nachricht, die wir schreiben möchten.

``` Haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "Dies ist eine Fehlermeldung."
  hPutStrLn stderr "Dies ist eine Warnung."
```

Die obigen Codezeilen werden bei der Ausführung des Programms die folgende Ausgabe auf der Konsole erzeugen:

```
Dies ist eine Fehlermeldung.
Dies ist eine Warnung.
```

## Tiefere Einblicke
In Haskell gibt es drei verschiedene Handles für den Standardausgabe-, Standardfehler- und Standardeingabestrom, die als `stdout`, `stderr` und `stdin` bezeichnet werden.

Jedes Mal, wenn wir `hPutStrLn` verwenden, wird die Nachricht in den angegebenen `Handle` geschrieben. Standardmäßig ist der `Handle` für `stderr` auf `stdout` gesetzt. Das bedeutet, dass die Ausgabemeldungen für Fehler und Warnungen standardmäßig auf der gleichen Konsole angezeigt werden, wie die Ausgabe unseres Programms.

Es ist jedoch wichtig zu beachten, dass sich die Handles während der Laufzeit des Programms ändern können. Daher ist es zu empfehlen, die Funktion `hSetBuffering` zu verwenden, um sicherzustellen, dass die Ausgabe auf dem richtigen `Handle` gespeichert wird.

## Siehe auch
- [Haskell-Dokumentation zu Handles](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-IO.html#t:Handle)
- [Haskell-Dokumentation zu Standardausgabe, Standardfehler und Standardeingabe](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-IO.html#v:stderr)
- [Official Haskell Website](https://www.haskell.org/)