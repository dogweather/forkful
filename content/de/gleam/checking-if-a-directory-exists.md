---
title:    "Gleam: Überprüfung, ob ein Verzeichnis vorhanden ist"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# Warum

Das Überprüfen, ob ein Verzeichnis existiert, ist ein wichtiger Teil des Programmierens. Es hilft dabei, sicherzustellen, dass das Programm auf die richtigen Dateien und Ordner zugreift und vermeidet fehlerhafte Ausführungen.

# Wie man es macht

Die Überprüfung, ob ein Verzeichnis existiert, ist in Gleam sehr einfach. Wir verwenden dafür die `file` Bibliothek. Zuerst importieren wir die Bibliothek in unserem Code:

```
Gleam
use file
```

Dann können wir die Funktion `file.exists()` verwenden, um zu überprüfen, ob ein bestimmtes Verzeichnis existiert. Hier ist ein Beispiel, das überprüft, ob das Verzeichnis "MeinOrdner" existiert und den entsprechenden Output ausgibt:

```
Gleam
fn do_check_my_dir() {
  if file.exists("MeinOrdner") == Ok ->
    Debug.log("MeinOrdner existiert!")
  if file.exists("MeinOrdner") == Error(err) ->
    Debug.log("MeinOrdner existiert nicht.")
}
```

# Tieferer Einblick

Die `file.exists()` Funktion gibt entweder `Ok` oder `Error` zurück, je nachdem, ob das Verzeichnis vorhanden ist oder nicht. Wenn `Ok` zurückgegeben wird, bedeutet das, dass das Verzeichnis existiert. `Error` wird zurückgegeben, wenn entweder das Verzeichnis nicht existiert oder ein anderer Fehler aufgetreten ist.

Ein weiterer wichtiger Aspekt bei der Überprüfung von Verzeichnissen ist die Reihenfolge, in der die Verzeichnisse überprüft werden. Mit der `file.exists()` Funktion können wir angeben, ob wir wollen, dass die Überprüfung auch in Unterordnern durchgeführt wird. Dazu verwenden wir das optionale Argument `recursive`, z.B. `file.exists("MeinOrdner", recursive=true)`.

# Siehe auch

- [Official Gleam Documentation on File Module](https://gleam.run/documentation/stdlib/file.html)
- [Gleam Community on Discord](https://discord.gg/8xjVAKZ)
- [Code Examples on Github](https://github.com/search?q=gleam+directory+exists&type=Repositories)