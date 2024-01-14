---
title:    "Elixir: Erstellen einer temporären Datei"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Warum

In der Welt der Softwareentwicklung kann es verschiedene Gründe geben, warum wir temporäre Dateien erstellen müssen. Möglicherweise müssen wir Daten zwischenspeichern oder generierte Dateien für bestimmte Prozesse erstellen. Elixir bietet eine einfache Möglichkeit, temporäre Dateien zu erstellen und zu verwalten. In diesem Blog-Beitrag werden wir uns genauer ansehen, wie wir dies tun können.

# Wie geht das?

Um in Elixir eine temporäre Datei zu erstellen, müssen wir die Funktion `Tempfile.create` aus dem Modul `Tempfile` aufrufen. Dies erfordert die Elixir-Standardbibliothek.

```Elixir
iex> Tempfile.create
{:ok, #PID<0.160.0>, %Tempfile{path: "...", fd: 5}}
```

Die Ausgabe dieser Funktion ist ein Tupel mit der atomaren Erfolgskennung `:ok`, der PID des Prozesses, der die temporäre Datei erstellt hat, und einer Map mit Informationen über die erstellte Datei.

Um die Datei zu verwenden, können wir einfach auf `path` in der Map zugreifen und die Datei wie gewohnt behandeln. Wir können auch die Funktion `Tempfile.close` verwenden, um die Datei zu schließen und zu löschen.

```Elixir
iex> {_, _, tempfile} = Tempfile.create
{:ok, #PID<0.162.0>, %Tempfile{path: "/tmp/tempfile20210203-2180-37uhm4.json", fd: 5}}
iex> file = File.open(tempfile.path, [:w])
#File<"/tmp/tempfile20210203-2180-37uhm4.json">
iex> IO.write(file, "Hello World!")
:ok
iex> file |> Tempfile.close()
:ok
```

# Tiefentauchen

Die Funktion `Tempfile.create` gibt uns die Möglichkeit, anpassbare Optionen zu übergeben. Eine dieser Optionen ist `prefix`, mit der wir den Dateinamen nach unseren Wünschen benennen können. Wir können auch die Option `dir` verwenden, um den Speicherort der Datei anzupassen.

```Elixir
iex> Tempfile.create(prefix: "test", dir: "/home/username")
{:ok, #PID<0.162.0>, %Tempfile{path: "/home/username/test20210203-21754-19tsd0u.json", fd: 5}}
```

Diese Funktion erstellt automatisch eine eindeutige Datei basierend auf der angegebenen Präfix und dem aktuellen Timestamp. Wir können auch die Option `suffix` verwenden, um eine bestimmte Dateiendung hinzuzufügen.

# Siehe auch

- Offizielle Dokumentation zur Tempfile-Modul: https://hexdocs.pm/elixir/Tempfile.html
- Weitere Informationen zum Thema Elixir: https://elixir-lang.org/