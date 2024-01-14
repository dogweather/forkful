---
title:    "Elixir: Unterstrings extrahieren"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# Warum
Die Extraktion von Teilstrings ist eine nützliche Fähigkeit in der Elixir-Programmierung. Es ermöglicht dir, gezielt nach bestimmten Teilen eines Strings zu suchen und diese zu extrahieren. Dies kann besonders wichtig sein, wenn du mit großen Datensätzen arbeitest oder Daten filtern musst.

# Wie
Um den Elixir-Code für die Extraktion von Teilstrings zu nutzen, gibt es eine Reihe von Funktionen, die du verwenden kannst.

```Elixir
# Gibt den Teilstring von Index 5 bis Ende des Strings zurück
String.slice("Hallo Welt", 5..-1)
# Ausgabe: "Welt"

# Gibt den Teilstring von Index 0 bis 3 zurück
String.slice("Hello World", 0, 3)
# Ausgabe: "Hel"

# Gibt den Nachnamen aus einem vollständigen Namen zurück
String.split("Max Mustermann", " ") |> List.last
# Ausgabe: "Mustermann"
```

# Deep Dive
Die Extraktion von Teilstrings kann auch in komplexeren Szenarien nützlich sein, wie z.B. bei der Suche nach Mustern in einem String. Hier kommt die Regex-Bibliothek von Elixir ins Spiel, die es dir ermöglicht, komplexe Ausdrücke zu verwenden, um Teilstrings zu finden und zu extrahieren.

```Elixir
# Gibt die erste Zeichenfolge zurück, die mit "Elixir" beginnt
Regex.run("Elixir ist eine tolle Programmiersprache", ~r/^Elixir/)
# Ausgabe: "Elixir"

# Gibt alle Zahlen aus einem String zurück
Regex.scan("42 ist die Antwort auf alles", ~r/\d+/) |> Enum.map(&hd/1)
# Ausgabe: ["42"]
```

# Siehe auch
- Elixir String-Dokumentation: https://hexdocs.pm/elixir/String.html
- Regex-Bibliothek für Elixir: https://hex.pm/packages/regex
- Eine Einführung in die Elixir-Programmierung: https://elixirschool.com/de/