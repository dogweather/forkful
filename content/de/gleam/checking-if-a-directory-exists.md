---
title:    "Gleam: Überprüfung der Existenz eines Ordners"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Warum

Das Überprüfen, ob ein Verzeichnis existiert, kann in der Programmierung sehr nützlich sein, da es sicherstellt, dass das Programm auf die erforderlichen Dateien und Ordner zugreifen kann. Dadurch wird vermieden, dass das Programm abstürzt oder unerwünschte Ergebnisse liefert.

## Wie geht das?

Es gibt verschiedene Möglichkeiten, um in Gleam zu prüfen, ob ein Verzeichnis existiert. Hier sind einige Beispiele mithilfe von Codeblöcken:

```Gleam

// Mit dem Befehl `os.dir_exists()` kann überprüft werden, ob das angegebene Verzeichnis vorhanden ist.
let verzeichnis = os.dir_exists("/home/benutzer/dokumente/")

if verzeichnis == Ok {
  // Das Verzeichnis existiert.
  io.print("Das Verzeichnis existiert.")
} else if verzeichnis == Err {
  // Das Verzeichnis existiert nicht.
  io.print("Das Verzeichnis existiert nicht.")
}

// Eine weitere Möglichkeit ist die Verwendung von Mustern. Hier wird auch ein Standardwert festgelegt, falls das Verzeichnis nicht gefunden wird.
case os.dir_exists("/home/benutzer/fotos/") {
  Ok -> { io.print("Das Verzeichnis existiert.") }
  Err -> { io.print("Das Verzeichnis existiert nicht.") }
  _ -> { io.print("Ein Fehler ist aufgetreten.") }
}

```

Die Ausgabe für beide Codebeispiele wäre: "Das Verzeichnis existiert."

## Tiefere Einblicke

Beim Überprüfen, ob ein Verzeichnis existiert, gibt es einige Dinge zu beachten. Zum einen muss das Verzeichnis richtig angegeben werden, damit das Programm es finden kann. Zum anderen ist es wichtig, sicherzustellen, dass das Programm entsprechende Berechtigungen hat, um auf das Verzeichnis zuzugreifen.

Um eine bessere Fehlerbehandlung zu gewährleisten, kann auch eine Error-Struktur verwendet werden, um spezifische Fehlermeldungen zu erhalten. Außerdem kann die Funktion `os.is_dir()` verwendet werden, um zu überprüfen, ob es sich bei dem angegebenen Pfad tatsächlich um ein Verzeichnis und nicht um eine Datei handelt.

# Siehe auch

- [`os.dir_exists()` Dokumentation](https://gleam.run/modules/gleam/os/#dir_exists)
- [Verzeichnisoperationen in Gleam](https://medium.com/@gleamlang/file-system-operations-f6275be4c1a3)
- [Einführung in Gleam für Anfänger (auf Deutsch)](https://ria.evolveu.dashboard.education/learn/gleamlang-for-beginners/)