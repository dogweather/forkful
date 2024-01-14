---
title:    "Elm: Erstellen einer temporären Datei"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Das Erstellen von temporären Dateien kann eine nützliche Technik sein, um Daten während der Ausführung eines Programms zu verarbeiten und zu speichern. Diese Dateien sind temporär und werden nach Beendigung des Programms automatisch gelöscht, was hilfreich sein kann, um Speicherplatz zu sparen.

## Wie man es macht

Eine temporäre Datei in Elm zu erstellen, ist relativ einfach. Zunächst importieren wir das `Temp` -Modul:

```Elm
import Temp
```

Als nächstes verwenden wir die `Temp.file` -Funktion, um eine temporäre Datei zu erstellen:

```Elm
Temp.file "test.txt"
```

Diese Funktion erstellt eine temporäre Datei mit dem angegebenen Namen und gibt ein `Result` zurück. Wir können dieses `Result` mithilfe von `case` auswerten und auf mögliche Fehler prüfen:

```Elm
case Temp.file "test.txt" of 
    Ok tempFile -> 
        -- Hier können wir die temporäre Datei verwenden 
    Err error -> 
        -- Hier können wir auf den Fehler reagieren 
```

Um Daten in die temporäre Datei zu schreiben, verwenden wir die `File.write` -Funktion und geben den Inhalt als `String` an:

```Elm
Temp.file "test.txt"
    |> Result.andThen (\tempFile -> File.write tempFile "Hallo, Welt!")
```

Und um die temporäre Datei zu löschen, verwenden wir die `File.cleanup` -Funktion:

```Elm
Temp.file "test.txt"
    |> Result.andThen (\tempFile -> File.cleanup tempFile)
```

## Tieferer Einblick

Beim Erstellen einer temporären Datei können wir optional auch weitere Parameter angeben, z.B. den Ordnerpfad und den Dateityp. Diese können uns bei der Verwaltung und Organisation unserer temporären Dateien helfen.

```Elm
Temp.file "temp/test.txt" "txt"
```

Außerdem ist es möglich, Filter und Vorlagen zu verwenden, um spezifischere und komplexere temporäre Dateien zu erstellen. Dazu können wir die Funktionen `Temp.fromTemplate` und `Temp.filter` verwenden.

```Elm
Temp.file (Temp.fromTemplate "temp-XXX") "txt"

Temp.file "temp/test.revisions.txt" 
    |> Result.andThen (\tempFile -> fileFilter >--> File.write tempFile)
```

## Siehe auch

- [Elm Dokumentation zu `Temp`](https://package.elm-lang.org/packages/elm/core/latest/Temp)
- [Offizielle Elm Webseite](https://elm-lang.org/)
- [Weitere Artikel auf dem Elm Blog](https://elm-lang.org/blog)