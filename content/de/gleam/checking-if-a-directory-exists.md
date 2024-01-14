---
title:                "Gleam: Überprüfen, ob ein Verzeichnis vorhanden ist"
simple_title:         "Überprüfen, ob ein Verzeichnis vorhanden ist"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Das Überprüfen, ob ein Verzeichnis existiert, ist eine wichtige Funktion in der Programmierung. Es ermöglicht uns, zu überprüfen, ob ein bestimmter Pfad vorhanden ist, bevor wir versuchen, Dateien in dieses Verzeichnis zu schreiben oder aus diesem zu lesen. Dies kann verhindern, dass unser Programm abstürzt oder unerwartete Fehler auftreten.

## Wie geht das?

Das Überprüfen der Existenz eines Verzeichnisses kann in Gleam auf verschiedene Arten erfolgen. Eine Möglichkeit ist die Verwendung der Funktion `:os.exists/1`, die einen booleschen Wert zurückgibt, der angibt, ob ein Pfad existiert oder nicht. Diese Funktion erfordert jedoch, dass sie mit dem absoluten Pfad zum Verzeichnis aufgerufen wird.

```Gleam
// Überprüfen Sie, ob das Verzeichnis "mein_verzeichnis" im aktuellen Verzeichnis existiert
let result = :os.exists("mein_verzeichnis")

// Überprüfen Sie, ob das Verzeichnis "/home/benutzer/mein_verzeichnis" existiert
let result = :os.exists("/home/benutzer/mein_verzeichnis")
```

In einigen Fällen ist es jedoch nicht praktikabel oder effizient, den absoluten Pfad zu kennen oder zu verwenden. In solchen Fällen können wir die Funktion `:os.cwd/0` verwenden, die den aktuellen Arbeitsverzeichnis-Pfad zurückgibt, und diesen mit dem gewünschten Verzeichnis-Pfad kombinieren. Wir müssen jedoch sicherstellen, dass der kombinierte Pfad immer absolut ist, indem wir die Funktion `:os.abs_path/1` verwenden.

```Gleam
// Überprüfen Sie, ob das Verzeichnis "mein_verzeichnis" im aktuellen Arbeitsverzeichnis existiert
let result =
  :os.cwd()
  |> IO.inspect("Aktuelles Arbeitsverzeichnis ist: ")
  |> :os.abs_path()
  |> IO.inspect("Absoluter Pfad des aktuellen Arbeitsverzeichnisses ist: ")
  |> :os.exists("mein_verzeichnis")
  |> IO.inspect("Existiert das Verzeichnis?")
```

Die Ausgabe dieses Codes könnte wie folgt aussehen:

```bash
Aktuelles Arbeitsverzeichnis ist: "/home/benutzer"
Absoluter Pfad des aktuellen Arbeitsverzeichnisses ist: "/home/benutzer"
Existiert das Verzeichnis? true
```

## Tiefere Einblicke

Das Überprüfen der Existenz eines Verzeichnisses mag auf den ersten Blick einfach erscheinen, aber es gibt einige Dinge, die man beachten muss. Zum Beispiel müssen wir sicherstellen, dass der angegebene Verzeichnis-Pfad existiert und auch ein Verzeichnis und kein Dateipfad ist. In Gleam können wir dies mit der Funktion `:os.is_dir/1` überprüfen, die ebenfalls einen booleschen Wert zurückgibt.

Zusätzlich kann es auch hilfreich sein, zu überprüfen, ob wir Schreib- oder Lesezugriff auf das Verzeichnis haben, bevor wir versuchen, Dateien darin zu erstellen oder zu lesen. Dafür können wir die Funktionen `:os.writable/1` bzw. `:os.readable/1` verwenden.

```Gleam
// Überprüfen Sie, ob das Verzeichnis "mein_verzeichnis" im aktuellen Arbeitsverzeichnis existiert und schreibbar ist
let result =
  :os.rwx()
  |> :os.abs_path()
  |> IO.inspect("Absoluter Pfad von meinem Arbeitsverzeichnis ist: ")
  |> :os.exists()
  |> :os.is_dir()
  |> IO.inspect("Existiert und ist es ein Verzeichnis?")
  |> :os.writable()
  |> IO.inspect("Ist es schreibbar?")
```

Die Ausgabe dieses Codes könnte wie folgt aussehen:

```bash
Absoluter Pfad von meinem Arbeitsverzeichnis ist: "/home/benutzer"
Existiert und ist es ein Verzeichnis? true
Ist es schreibbar? true
```

## Siehe auch

- [Gleam Dokumentation zu Dateioperationen](https://gleam.run/book/stdlib.html#file-operations)
- [Gleam Standardbibliothek Dokumentation](https://gleam.run/docs/stdlib/os.html)
- [Gleam-Tutorial: Einführung in das Arbeiten mit Dateien und Verzeichnissen](https://dev.to/gleam_lang/tutorial-working-with