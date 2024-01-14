---
title:                "Haskell: Lesen einer Textdatei"
simple_title:         "Lesen einer Textdatei"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Warum
In der Welt der Programmierung gibt es oft Szenarien, in denen wir Textdateien analysieren oder bearbeiten müssen. In dieser Anleitung werden wir uns mit dem Lesen von Textdateien mit Haskell befassen und wie dies in unsere Programmierprojekte integriert werden kann. Wenn Sie also daran interessiert sind, mehr über das Lesen von Textdateien mit Haskell zu erfahren, sind Sie hier genau richtig!

# Wie man 
In der Welt von Haskell gibt es mehrere Möglichkeiten, Textdateien zu lesen. Eine einfache Methode ist die Verwendung der `readFile` Funktion. Schauen wir uns ein Beispiel an, wie wir diese Funktion verwenden können:

```Haskell
import System.IO

main = do
    handle <- openFile "beispiel.txt" ReadMode
    contents <- hGetContents handle
    putStrLn contents
    hClose handle
```

Wenn wir diesen Code ausführen, wird der Text der Datei "beispiel.txt" in der Konsole ausgegeben. Wir können auch die `getContents` Funktion verwenden, um Text von der Standard-Eingabe zu lesen:

```Haskell
main = do
    putStrLn "Geben Sie einen Text ein:"
    contents <- getContents
    putStrLn contents
```

Jetzt können wir Text von der Konsole eingeben und sehen, wie Haskell ihn ausgibt.

# Tiefentauchen
Jetzt, da wir einen grundlegenden Überblick über das Lesen von Textdateien in Haskell haben, können wir uns etwas tiefer damit beschäftigen. Neben `readFile` und `getContents` gibt es noch andere Funktionen, die uns beim Lesen von Textdateien helfen können. Eine davon ist die `lines` Funktion, die eine Textdatei in Zeilen aufteilt. Lassen Sie uns ein Beispiel sehen:

```Haskell
main = do
    handle <- openFile "beispiel.txt" ReadMode
    contents <- hGetContents handle
    let linesOfFile = lines contents  -- Teilt die Textdatei in Zeilen auf
    print linesOfFile
    hClose handle
```

Ein weiterer nützlicher Aspekt beim Lesen von Textdateien in Haskell ist die Verwendung von `Text.IO` anstelle von `System.IO`. `Text.IO` bietet zusätzliche Funktionen für die Verarbeitung von Textdateien, wie z.B. die `hGetLine` Funktion, die eine Zeile aus einer Textdatei liest.

# Siehe auch
- [Offizielle Haskell-Dokumentation](https://www.haskell.org/documentation/)
- [Tutorial: Textverarbeitung in Haskell](https://www.schoolofhaskell.com/user/commercial/content/text-processing-in-haskell)
- [Eine praktische Einführung in Haskell](http://learnyouahaskell.com/chapters)