---
title:    "Gleam: Erstellen einer temporären Datei"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Die Erstellung temporärer Dateien ist oft eine notwendige Aufgabe beim Programmieren. Hier erfährst du, warum diese Funktion so wichtig ist und wie du sie in Gleam implementieren kannst.

## Wie geht das?

Die Erstellung einer temporären Datei in Gleam ist ganz einfach. Zuerst müssen wir das `os` Modul importieren, welches uns Zugriff auf Betriebssystem-Funktionen gibt.

```
Gleam module TempFile {

    import os

    fn create_temp_file() {
        let filename = os.tmp()
        // Hier können wir dann die Datei manipulieren
    }
}
```

Die Funktion `os.tmp()` gibt uns einen String zurück, welcher der Pfad und Name der temporären Datei ist. Wir können dann diesen String verwenden, um die Datei zu erstellen und zu bearbeiten.

## Tiefer Einblick

Manchmal möchtest du vielleicht mehr Kontrolle über deine temporären Dateien haben, zum Beispiel indem du bestimmte Berechtigungen festlegst oder einen benutzerdefinierten Dateinamen verwendest. In Gleam gibt es die Funktion `os.tmp_with_options()`, die uns die Möglichkeit gibt, diese Parameter selbst anzugeben.

```
Gleam module TempFile {

    import os

    type FileOptions {
        permissions: Int,
        prefix: String,
        suffix: String
    }

    pub fn create_temp_file(options: FileOptions) {
        let filename = os.tmp_with_options(options)
        // Hier können wir dann die Datei manipulieren
    }
}
```

Wir definieren eine eigene Datentyp `FileOptions`, der die entsprechenden Parameter enthält, und übergeben diese dann an die `os.tmp_with_options()` Funktion. Dies gibt uns noch mehr Flexibilität bei der Erstellung von temporären Dateien.

## Siehe auch

- [Gleam Dokumentation - os Modul](https://gleam.run/modules/standard-libraries/os/)
- [Gleam Greeter Tutorial](https://github.com/gleam-lang/gleam/blob/main/docs/getting-started/greeter.md) (Deutsch)
- [Einführung in Gleam](https://dev.to/m1garand/getting-started-with-gleam-dm7) (Deutsch)