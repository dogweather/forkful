---
title:                "Haskell: Eine Textdatei lesen"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Warum
Manchmal müssen wir Programmierer Textdateien lesen und analysieren, sei es für die Verarbeitung von Benutzereingaben oder für die Datenanalyse. In diesem Blogpost werden wir uns ansehen, wie wir Textdateien in Haskell lesen können und warum es wichtig ist, diese Fähigkeit zu beherrschen.

## Wie man Textdateien in Haskell liest
Das Lesen von Textdateien in Haskell ist ein relativ einfacher Prozess, der mit wenigen Codezeilen erledigt werden kann. Zunächst müssen wir die "Data.Text" Bibliothek importieren, um die Textdatei zu lesen. Dann können wir die Funktion "readFile" verwenden, um die ganze Datei auf einmal zu lesen oder "readFileLines", um die Datei Zeile für Zeile zu lesen. Hier ist ein Beispiel, wie man eine Textdatei mit "readFile" liest:

```Haskell
import Data.Text (pack, unpack)

main = do
    content <- readFile "textdatei.txt"
    putStrLn (unpack content)
```

Die Funktion "readFile" gibt einen "IO Text" Wert zurück, also müssen wir die Funktion "unpack" benutzen, um den Inhalt der Datei in einen String umzuwandeln. Dann können wir den Inhalt einfach mit "putStrLn" drucken. Wenn wir die Datei Zeile für Zeile lesen möchten, können wir "readFileLines" verwenden:

```Haskell
import Data.Text (unpack)

main = do
    content <- readFileLines "textdatei.txt"
    mapM_ (putStrLn . unpack) content
```

Die Funktion "mapM_" wendet die Funktion "putStrLn" auf jede Zeile in der Textdatei an.

## Tiefergehende Untersuchung
Beim Lesen von Textdateien sollten wir auch auf die Dateicodierung achten. Standardmäßig wird ASCII als Codierung verwendet, aber in der Realität können Textdateien in verschiedenen Codierungen vorliegen. Um dies zu berücksichtigen, können wir die "Data.Text.IO" Bibliothek nutzen und die Funktion "readFileWith" implementieren, die die Dateicodierung sowie den Pfad der Textdatei als Parameter annimmt.

```Haskell
import Data.Text.IO (readFileWith)
import Data.Text.Encoding (decodeUtf8)

main = do
    content <- readFileWith decodeUtf8 "textdatei.txt"
    putStrLn (unpack content)
```

Hier verwenden wir die "decodeUtf8" Funktion, um die Datei in UTF-8 zu decodieren, aber dies kann angepasst werden, je nachdem welche Codierung die Datei hat.

# Siehe auch
- [Haskell Dokumentation über das Lesen von Textdateien](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Text.html)
- [Einführung in Haskell von einer deutschen Universität](https://github.com/Blaze2305/HaskellVorlesung/blob/master/slides/02-body.markdown#files)