---
title:    "Haskell: Eine Textdatei schreiben"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Warum
Haskell ist eine funktionale Programmiersprache, die auf dezidierte Typisierung und starke statische Typen setzt. Diese Eigenschaften machen Haskell zu einer robusten und sicheren Wahl für die Erstellung von Textdateien.

## Anleitung
Um eine Textdatei in Haskell zu schreiben, benötigen wir zunächst eine Importanweisung für das Modul "System.IO". Danach können wir die Funktion "writeFile" verwenden, die uns die Möglichkeit gibt, einen Dateinamen und den Inhalt der Datei anzugeben.

```Haskell
import System.IO
main = do 
    let filename = "meine-datei.txt" 
    let content = "Dies ist eine Beispieltextdatei." 
    writeFile filename content 
```

Dieser Code erstellt eine Datei namens "meine-datei.txt" im aktuellen Verzeichnis und schreibt den Inhalt "Dies ist eine Beispieltextdatei." hinein.

## Tiefere Einblicke
Neben der "writeFile" Funktion gibt es auch andere Möglichkeiten, um Textdateien in Haskell zu schreiben. Beispielsweise können wir die Funktion "appendFile" verwenden, um Inhalt an eine bereits bestehende Datei anzuhängen.

```Haskell
import System.IO
main = do 
    let filename = "meine-datei.txt" 
    let content = "Dies ist weiterer Text, der angehängt wird." 
    appendFile filename content
```

Wir können auch angeben, welchen Modus wir für das Öffnen der Datei verwenden möchten, z.B. Schreiben, Anhängen oder Lesen. Dies kann mit der Funktion "openFile" erreicht werden.

```Haskell
import System.IO
main = do 
    let filename = "meine-datei.txt" 
    let mode = WriteMode 
    let content = "Dies ist eine Beispieltextdatei." 
    file <- openFile filename mode
    hPutStrLn file content 
    hClose file
```

In diesem Beispiel verwenden wir den Modus "WriteMode", um die Datei zu öffnen und den Inhalt mit der Funktion "hPutStrLn" hineinzuschreiben. Am Ende müssen wir die Datei mit "hClose" schließen.

## Siehe auch 
- Offizielle Haskell-Dokumentation zum Schreiben von Dateien: https://www.haskell.org/documentation #write-ands-read-files
- Ein Tutorial zum Schreiben von Textdateien in Haskell: https://dev.to/ben/beginner-s-guide-to-the-io-monad-in-haskell-4k8n