---
title:    "Haskell: Schreiben auf Standardfehler"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Warum 

Das Schreiben von Programmen ist eine kreative und komplexe Aufgabe, die ständige Aufmerksamkeit und Verbesserung erfordern. Eine Möglichkeit, Ihren Code effektiver zu gestalten, ist die Verwendung von Standard Error. Durch das Schreiben von Code auf die Standard Error-Ausgabe werden Fehlermeldungen und Debugging-Informationen direkt in die Konsole geschrieben. Dies kann besonders nützlich sein, wenn Sie an komplexen Projekten arbeiten und Probleme schnell erkennen und lösen müssen.

## So geht's

Um Code auf die Standard Error-Ausgabe zu schreiben, müssen Sie die Funktion `hPutStrLn` aus dem Standardmodul `System.IO` importieren. Diese Funktion erwartet zwei Parameter: einen Handle und eine Zeichenkette. Der Handle gibt an, wohin die Zeichenkette geschrieben werden soll, und die Zeichenkette selbst ist der Inhalt, der geschrieben werden soll.

Ein Beispiel für das Schreiben auf die Standard Error-Ausgabe könnte wie folgt aussehen:

```Haskell
import System.IO (hPutStrLn, stderr)

main = do
    hPutStrLn stderr "Dies ist ein Beispiel für die Standard Error-Ausgabe."
```

Die Ausgabe dieses Codes würde in der Konsole wie folgt aussehen:

```
Dies ist ein Beispiel für die Standard Error-Ausgabe.
```

Sie können auch Variablen oder Ausdrücke anstelle der Zeichenkette in die Funktion `hPutStrLn` übergeben. Die Ausgabe wird dann entsprechend angepasst.

## Tiefere Einblicke 

Wenn Sie sich wirklich vertiefen möchten, gibt es noch weitere Dinge zu beachten, wenn Sie Code auf die Standard Error-Ausgabe schreiben. Ein wichtiger Punkt ist, dass die Standard Error-Ausgabe nicht dasselbe ist wie die Standardausgabe. Während die Standardausgabe normalerweise auf dem Bildschirm ausgegeben wird, ist die Standard Error-Ausgabe dafür gedacht, Fehlermeldungen und Debugging-Informationen auszugeben, die von anderen Programmen oder der Shell ausgewertet werden können.

Eine andere wichtige Überlegung ist die Reihenfolge, in der Ihre Ausgaben erscheinen. Da die Standard Error-Ausgabe für Fehlermeldungen und Debugging-Informationen vorgesehen ist, sollten Sie sicherstellen, dass diese Ausgaben vor allen anderen Ausgaben erscheinen, damit Sie schnell auf Probleme reagieren können.

Um dies zu erreichen, können Sie das Modul `System.IO` verwenden, um ein Handle für die Standard Error-Ausgabe zu erstellen und dann den `stdout` durch den `stderr` zu ersetzen, bevor Sie Ihren Code schreiben:

```Haskell
import System.IO (hPutStrLn, stderr, stdout, hDuplicate)

main = do
    errHandle <- hDuplicate stderr
    hDuplicateTo errHandle stdout
    hPutStrLn stderr "Dieser Text erscheint als Standard Error!"
    putStrLn "Dieser Text erscheint aber als Standardausgabe"
```

Diese Reihenfolge hilft Ihnen dabei, Fehler und andere wichtige Informationen sofort zu sehen, ohne dass sie von anderen Ausgaben überdeckt werden.

# Siehe auch 

- [Haskell Standardbibliothek: System.IO](https://hackage.haskell.org/package/base/docs/System-IO.html)
- [Haskell-Dokumentation: Standard Error-Ausgabe schreiben](https://www.haskell.org/onlinereport/io.html#hputstr)
- [Blogpost: Debugging mit Haskell](https://bartoszmilewski.com/2015/03/25/debugging-with-haskell/)