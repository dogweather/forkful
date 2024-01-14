---
title:    "Haskell: Überprüfen, ob ein Verzeichnis existiert"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum
Das Überprüfen, ob ein Verzeichnis existiert, kann hilfreich sein, um sicherzustellen, dass unser Programm auf die richtigen Dateien zugreift und um Fehler zu vermeiden.

## Anleitung
Um in Haskell zu prüfen, ob ein Verzeichnis existiert, können wir die Funktion `doesDirectoryExist` aus dem Modul `System.Directory` verwenden.

```Haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
    dirExists <- doesDirectoryExist "pfad/zum/verzeichnis"
    if dirExists
        then putStrLn "Das Verzeichnis existiert."
        else putStrLn "Das Verzeichnis existiert nicht."
```

Die Ausgabe wird je nach Ergebnis entweder "Das Verzeichnis existiert." oder "Das Verzeichnis existiert nicht." sein.

## Tiefentauchen
Die Funktion `doesDirectoryExist` führt eine systemabhängige Operation aus. Das bedeutet, dass die Überprüfung auf Windows möglicherweise anders funktioniert als auf Linux oder macOS. Bei der Verwendung dieser Funktion sollten wir daher darauf achten, dass unser Code plattformunabhängig ist.

## Siehe auch
- [Haskell Dokumentation zu `System.Directory`](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Ein Beispiel zur Verwendung von `doesDirectoryExist`](https://wiki.haskell.org/How_to_check_if_a_given_file_or_directory_exists_in_Haskell)