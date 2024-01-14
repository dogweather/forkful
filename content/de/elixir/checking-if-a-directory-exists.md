---
title:    "Elixir: Überprüfung, ob ein Verzeichnis existiert"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Das Überprüfen, ob ein Verzeichnis existiert, ist ein wichtiger Aspekt der Elixir-Programmierung. Durch diese Kontrolle können Programmierer sicherstellen, dass ihre Anwendungen auf die benötigten Dateien und Ordner zugreifen können. Dies kann auch dazu beitragen, potenzielle Fehler oder Ausfälle zu vermeiden.

## How To

```Elixir
defp dir_exists?(dir_path) do
  File.dir?(dir_path)
end

IO.puts dir_exists?("/Users/username/Documents") # Ausgabe: true
IO.puts dir_exists?("/Users/username/Desktop") # Ausgabe: true
IO.puts dir_exists?("/Users/username/Screenshots") # Ausgabe: false
```

In diesem Beispiel nutzen wir die Funktion `File.dir?`, um zu überprüfen, ob ein Verzeichnis unter dem angegebenen Pfad existiert. Diese Funktion gibt entweder `true` oder `false` zurück, abhängig davon, ob das Verzeichnis vorhanden ist oder nicht.

## Deep Dive

Für eine tiefere Betrachtung der Überprüfung der Existenz eines Verzeichnisses gibt es einige wichtige Aspekte, die beachtet werden sollten.

Zunächst ist es wichtig zu wissen, dass die Funktion `File.dir?` nicht nur die lokale Dateistruktur überprüft, sondern auch auf andere Dateisysteme zugreifen kann, wie zum Beispiel Netzlaufwerke oder Cloud-Dienste.

Des Weiteren ist es wichtig zu beachten, dass die Funktion `File.dir?` auch Symlinks überprüft. Dies bedeutet, dass, selbst wenn ein Verzeichnis unter einem bestimmten Pfad nicht existiert, aber ein Symlink zu diesem Verzeichnis vorhanden ist, die Funktion `true` zurückgibt.

Abschließend ist es wichtig zu beachten, dass die Überprüfung, ob ein Verzeichnis existiert, ein wichtiger zusammenhängender Prozess ist. Sie sollte daher immer in Verbindung mit anderen Funktionen und Kontrollen verwendet werden, um sicherzustellen, dass das Programm reibungslos funktioniert.

## Siehe auch

- [Offizielle Elixir-Dokumentation über Datei- und Verzeichnisoperationen](https://hexdocs.pm/elixir/File.html)
- [Elixir Forum Diskussion über das Überprüfen der Existenz eines Verzeichnisses](https://elixirforum.com/t/check-if-directory-exists/5532)
- [Elixir School Tutorial zu Dateien und Verzeichnissen](https://elixirschool.com/de/lessons/advanced/files/)