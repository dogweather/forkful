---
title:                "Erstellen einer temporären Datei"
html_title:           "Haskell: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man eine temporäre Datei erstellen wollen? Nun, temporäre Dateien sind nützlich, wenn man vorübergehend Daten speichern möchte, die später nicht mehr benötigt werden. Zum Beispiel können sie in der Programmierung verwendet werden, um Zwischenergebnisse zu speichern oder zum Testen von Funktionen, ohne dass es Auswirkungen auf die eigentliche Datei hat.


## Wie geht man vor

Das Erstellen einer temporären Datei in Haskell ist ganz einfach. Es gibt mehrere Möglichkeiten, aber hier sind zwei Beispiele:

```Haskell
import System.IO.Temp -- Dieses Modul muss importiert werden
import System.IO     -- Für die Verwendung von `hPutStrLn`

main = do
  -- Mit `withSystemTempFile`
  withSystemTempFile "temp.txt" $ \path handle -> do
    hPutStrLn handle "Hallo Welt!"
    hClose handle
    putStrLn $ "Temporäre Datei erstellt unter: " ++ path

  -- Mit `withTempFile`
  withTempFile "." "temp.txt" $ \path handle -> do
    hPutStrLn handle "Hello World!"
    hClose handle
    putStrLn $ "Temporäre Datei erstellt unter: " ++ path
```

Das obige Beispiel verwendet die Funktionen `withSystemTempFile` und `withTempFile` aus dem Modul `System.IO.Temp`. Diese Funktionen erstellen eine temporäre Datei im angegebenen Verzeichnis und führen eine Aktion aus, die auf das Handle der Datei zugreifen kann. Sobald die Aktion beendet ist, wird die Datei automatisch gelöscht. Um die Datei manuell zu löschen, kann die Funktion `removeFile` aus dem Modul `System.Directory` verwendet werden.

Der erste Parameter der Funktionen ist das Verzeichnis, in dem die temporäre Datei erstellt wird. Wenn es mit `Nothing` angegeben wird, wird das Standard-Temp-Verzeichnis des Betriebssystems verwendet. Der zweite Parameter ist der Dateiname, der verwendet wird. Und der dritte Parameter ist die Aktion, die auf das Handle zugreifen kann.


## Tiefer eintauchen

Es gibt viele weitere nützliche Funktionen im Modul `System.IO.Temp`, die verwendet werden können, um temporäre Dateien zu erstellen und zu verwalten. Ein paar Beispiele sind:

- `getCanonicalTemporaryDirectory` - gibt das Standard-Temp-Verzeichnis des Betriebssystems zurück
- `withTempDirectory` - erstellt ein temporäres Verzeichnis statt einer Datei
- `createTempDirectory` - erstellt ein temporäres Verzeichnis ohne Aktion
- `createTempFile` - erstellt eine temporäre Datei ohne Aktion, aber gibt den Pfad zurück

Weitere Informationen finden Sie in der offiziellen Dokumentation zu [`System.IO.Temp`](https://hackage.haskell.org/package/temporary/docs/System-IO-Temp.html).


## Siehe auch

- [`System.IO.Temp` Dokumentation](https://hackage.haskell.org/package/temporary/docs/System-IO-Temp.html)
- [`System.Directory` Dokumentation](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Haskell Cookbook - Working with Temp Files](https://haskell-cookbook.com/working-with-temp-files.html)

Happy Coding!