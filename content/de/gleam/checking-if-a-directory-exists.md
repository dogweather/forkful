---
title:                "Überprüfen, ob ein Verzeichnis vorhanden ist"
html_title:           "Gleam: Überprüfen, ob ein Verzeichnis vorhanden ist"
simple_title:         "Überprüfen, ob ein Verzeichnis vorhanden ist"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Warum

Hast du dich jemals gefragt, wie du in deinem Code überprüfen kannst, ob ein bestimmtes Verzeichnis existiert? In diesem Artikel werden wir uns ansehen, wie du das mit Gleam ganz einfach machen kannst.

## Wie geht das?

Um zu überprüfen, ob ein Verzeichnis existiert, können wir die `fs` Bibliothek von Gleam verwenden. Diese enthält die Funktion `exists`, die wir aufrufen können und als Argument den Pfad des Verzeichnisses übergeben.

```Gleam
import fs

let path = "/home/user/files"
let exists = fs.exists(path)
```

Das gibt uns ein `Result`-Objekt zurück, das entweder `Ok(true)` oder `Ok(false)` sein kann. Wir können dieses mit einem `case`-Statement verarbeiten, um je nachdem ob das Verzeichnis existiert oder nicht, verschiedene Aktionen auszuführen.

```Gleam
case exists {
    Ok(true) -> println("Das Verzeichnis existiert!")
    Ok(false) -> println("Das Verzeichnis existiert nicht.")
    Err(err) -> println("Ein Fehler ist aufgetreten: {err}")
}
```

## Tiefer tauchen

Bei der Verwendung von `fs.exists` müssen wir beachten, dass es nur auf lokalen Dateisystemen funktioniert. Wenn wir überprüfen wollen, ob ein Verzeichnis auf einem entfernten Server vorhanden ist, müssen wir auf andere Bibliotheken und Protokolle zugreifen.

Außerdem können wir auch andere Funktionen aus der `fs` Bibliothek verwenden, wie z.B. `create_dir`, um ein Verzeichnis zu erstellen, oder `list`, um alle Dateien und Unterverzeichnisse in einem Verzeichnis aufzulisten.

## Siehe auch

- [Gleam Dokumentation zu `fs.exists`](https://gleam.run/modules/gleam/fs/latest/api)
- [Tutorial: Working with Files in Gleam](https://dev.to/patyhuertas/tutorial-working-with-files-in-gleam-361g) (Englisch)