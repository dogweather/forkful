---
title:    "Elixir: Lesen einer Textdatei"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Textdateien ist ein grundlegender Bestandteil der Programmierung und kann in vielen Situationen nützlich sein. Zum Beispiel könnte man eine Textdatei als Teil eines Datenbankimports verwenden oder eine Benutzereingabe aus einer Datei lesen.

## Wie man es macht

Zum Lesen einer Textdatei in Elixir verwenden wir die `File.stream!` Funktion und eine Schleife, um jede Zeile zu verarbeiten. Zunächst müssen wir die Datei mit dem gewünschten Dateipfad öffnen, indem wir `:file.open` verwenden. Dann verwenden wir `File.stream!`, um einen Datenstrom von der Datei zu erstellen und `Enum.each` um jede Zeile zu durchlaufen. Schließlich schließen wir die Datei mit `:file.close`.

```Elixir
{:ok, file} = :file.open("dateiname.txt")
stream = IO.stream(file)
Enum.each(stream, fn line ->
  # Hier kommen Code-Aktionen für jede Zeile hin
end)
:file.close(file)
```

## Tieferer Einblick

Wenn wir eine sehr große Textdatei haben, kann es effizienter sein, `File.stream!` durch `Stream.resource` zu ersetzen. Dies gibt uns die Möglichkeit, Ressourcen manuell zu verwalten und die Performance zu optimieren.

```Elixir
def file_stream(path) do
  {:ok, file} = :file.open(path)
  stream = IO.stream(file)
  Stream.resource(
    fn ->
      stream # Der Datenstrom wird als Ressource zurückgegeben
    end,
    fn _ ->
      :file.close(file) # Die Datei wird automatisch geschlossen, wenn der Datenstrom beendet wird
    end,
    fn stream ->
      case IO.binread(stream, 1024) do # Wir lesen die Datei blockweise
        <<line::binary-size(1024), rest::binary>> ->
          {line, rest}
        <<line::binary>> ->
          {line, ""}
      end
    end
  )
end
```

Jetzt können wir `file_stream` in unserem Code verwenden:

```Elixir
file_stream("dateiname.txt")
|> Enum.each(fn line ->
  # Hier kommen Code-Aktionen für jede Zeile hin
end)
```

## Siehe auch

- [Elixir Dokumentation zu Textdateien lesen/schreiben](https://hexdocs.pm/elixir/Kernel.SpecialForms.html#stream!/2)
- [Elixir Dokumentation zu Dateien](https://hexdocs.pm/elixir/Kernel.File.html)
- [Elixir Code-Beispiele auf GitHub](https://github.com/elixir-lang/elixir/search?utf8=%E2%9C%93&q=file&type=Code)