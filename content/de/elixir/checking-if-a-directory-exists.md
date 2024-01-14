---
title:    "Elixir: Überprüfen, ob ein Verzeichnis existiert"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# Warum

In der Welt der Programmierung ist es oft notwendig zu überprüfen, ob ein bestimmtes Verzeichnis existiert oder nicht. In diesem Blog-Beitrag werden wir uns genauer ansehen, wie man dies in Elixir tun kann.

# Wie es geht

Um zu überprüfen, ob ein Verzeichnis existiert, können wir die Funktion `File.dir?/1` verwenden. Diese Funktion akzeptiert einen Dateipfad als Argument und gibt `true` zurück, wenn das Verzeichnis existiert, oder `false`, wenn es nicht existiert.

```Elixir
# Beispiel 1: Überprüfung eines vorhandenen Verzeichnisses
File.dir?("/home/user/Documents")

# Ausgabe: true
```

```Elixir
# Beispiel 2: Überprüfung eines nicht vorhandenen Verzeichnisses
File.dir?("/home/user/Desktop")

# Ausgabe: false
```

# Tieferer Einblick

Das Überprüfen, ob ein Verzeichnis vorhanden ist, kann nützlich sein, um verschiedene Aktionen in unserer Elixir-Anwendung basierend auf verschiedenen Dateipfaden auszuführen. Es kann auch Teil von Fehlerbehandlungsroutinen sein, um sicherzustellen, dass Dateien oder Verzeichnisse existieren, bevor wir versuchen, mit ihnen zu arbeiten.

Es ist wichtig zu beachten, dass die Funktion `File.dir?/1` nur prüft, ob ein Verzeichnis unter dem angegebenen Pfad existiert. Es überprüft nicht, ob der angegebene Pfad selbst ein Verzeichnis ist. Um das zu tun, können wir `File.is_dir?/1` verwenden.

Die Verwendung von `File.dir?/1` und `File.is_dir?/1` kann auch hilfreich sein, um sicherzustellen, dass unsere Elixir-Anwendung auf verschiedenen Betriebssystemen einheitlich funktioniert. Ein Beispiel ist, dass `File.dir?/1` auf Windows-Systemen auch zurückgeben kann, dass eine Datei existiert, während `File.is_dir?/1` in diesem Fall `false` zurückgeben würde.

# Siehe auch

- Offizielle Elixir-Dokumentation zur `File`-API: https://hexdocs.pm/elixir/File.html
- Blogbeitrag über den Umgang mit Dateien und Verzeichnissen in Elixir: https://blog.lelonek.me/handling-files-and-directories-in-elixir-fa334603340
- Einführung in Elixir: https://medium.com/@allenlin26/introduction-to-elixir-programming-language-ad6797e6c320