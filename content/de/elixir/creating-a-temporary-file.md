---
title:                "Elixir: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Warum

Es gibt viele Gründe, warum man in der Elixir-Programmierung temporäre Dateien erstellen könnte. Zum Beispiel können temporäre Dateien verwendet werden, um Daten zwischen verschiedenen Funktionen oder Prozessen auszutauschen oder um temporäre Speicherung für bestimmte Dateioperationen bereitzustellen.

# Wie man eine temporäre Datei erstellt

Um eine temporäre Datei in Elixir zu erstellen, können wir die Funktion `:os.tmpname()` verwenden. Diese Funktion gibt uns einen zufälligen Dateinamen zurück, der nicht bereits in unserem Dateisystem existiert.

```
Elixir
{:ok, file_path} = :os.tmpname()
IO.puts(file_path)
# Output: "/var/folders/qw/9zs8s4dj73v8jt_rvmt5kjv94kc8ml/T/tempfile_73421"
```

Wir können dann die Funktion `File.open/2` verwenden, um die temporäre Datei zu erstellen und von dort aus weiterzuarbeiten.

```
Elixir
File.open(file_path, [:write], fn file ->
  # Hier können wir Daten in die temporäre Datei schreiben
  IO.write(file, "Hello World!")
end)
```

# Tiefer Einblick

Wenn wir uns genauer mit dem Erstellen von temporären Dateien beschäftigen, sollten wir beachten, dass die Datei möglicherweise nicht sofort gelöscht wird, wenn wir fertig damit sind. Das liegt daran, dass der Dateiname immer noch gespeichert wird und somit Platz in unserem Dateisystem beansprucht. Um dies zu vermeiden, können wir die Funktion `File.unlink/1` verwenden, um die temporäre Datei manuell zu löschen.

```
Elixir
File.open(file_path, [:write], fn file ->
  # Hier können wir Daten in die temporäre Datei schreiben
  IO.write(file, "Hello World!")
end)

# Löschen Sie die temporäre Datei
File.unlink(file_path)
```

# Siehe auch

- [Offizielle Elixir Dokumentation](https://hexdocs.pm/elixir/File.html#open/2)
- [Elixir Cookbook - Working with Temporary Files](https://elixircasts.io/working-with-temporary-files-in-elixir)