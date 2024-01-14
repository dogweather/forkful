---
title:                "Haskell: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Warum

In der Welt des Programmierens gibt es oft Situationen, in denen es notwendig ist, temporäre Dateien zu erstellen. Diese Dateien dienen als Zwischenspeicher und können für verschiedene Zwecke wie das Speichern von Benutzereingaben oder das Verarbeiten von Daten verwendet werden. In diesem Blog-Beitrag werden wir uns genauer anschauen, warum es manchmal notwendig ist, temporäre Dateien in Haskell zu erstellen und wie man dies am besten durchführt.

# Wie erstelle ich eine temporäre Datei?

Das Erstellen einer temporären Datei in Haskell ist ziemlich einfach und erfordert nur wenige Zeilen Code. Zunächst müssen wir das temporäre Modul von Haskell importieren, indem wir ```import System.IO.Temp``` in unserem Code verwenden. Danach können wir mit Hilfe der Funktion ```withSystemTempFile``` eine temporäre Datei erstellen. Diese Funktion nimmt als Parameter eine Aktion, die auf der temporären Datei ausgeführt werden soll, sowie einen Dateipfad als Rückgabewert.

Um dies besser zu verstehen, schauen wir uns ein Beispiel an:

```Haskell
import System.IO.Temp

main = withSystemTempFile "temp.txt" $ \path handle -> do
    hPutStrLn handle "Dies ist eine temporäre Datei."
    putStrLn $ "Die temporäre Datei befindet sich unter: " ++ path
```

Hier erstellen wir eine temporäre Datei mit dem Namen "temp.txt" und lassen sie innerhalb unserer Aktion schreiben. Nachdem die Aktion abgeschlossen ist, wird die temporäre Datei automatisch gelöscht. Der Dateipfad wird als zweites Argument an unsere Aktion übergeben, so dass wir ihn verwenden können, um den Speicherort unserer temporären Datei zu drucken.

Die Ausgabe dieses Codes sieht folgendermaßen aus:

```
Die temporäre Datei befindet sich unter: /tmp/temp3095374167.txt
```

Durch die Verwendung der Funktion ```hPutStrLn``` können wir unseren Text in die temporäre Datei schreiben. Beachten Sie, dass wir das Handle der temporären Datei als Argument an die Funktion übergeben müssen. Nachdem die Aktion abgeschlossen ist, wird das Handle automatisch geschlossen und die temporäre Datei gelöscht.

# Tieferer Einblick

Das temporäre Modul von Haskell bietet auch andere nützliche Funktionen wie ```withSystemTempDirectory```, mit der wir temporäre Verzeichnisse erstellen und verwenden können. Außerdem gibt es die Möglichkeit, die Standard-Optionen für das Erstellen von temporären Dateien zu ändern, wie z. B. den Speicherort oder das Präfix der erstellten Datei.

Es ist auch wichtig zu erwähnen, dass es in Haskell möglich ist, die Standard-Optionen von temporären Dateien für den gesamten Code oder nur für bestimmte Bereiche zu ändern. Dies ermöglicht eine größere Flexibilität und Kontrolle über die temporären Dateien in Ihrem Code.

# Siehe auch

- [Haskell Dokumentation zu temporären Dateien](https://downloads.haskell.org/~ghc/6.10.1/docs/html/libraries/base-4.2.0.0/System-IO.html#19)
- [Beispielcode auf GitHub](https://github.com/example/example)