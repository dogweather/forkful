---
title:                "Elixir: Erstellen einer temporären Datei"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum
In der Welt der Elixir Programmierung gibt es viele nützliche Funktionen, die bei der Entwicklung von Anwendungen hilfreich sein können. Eine solche Funktion ist die Erstellung einer temporären Datei. Warum sollte man also die Mühe auf sich nehmen, eine temporäre Datei zu erstellen?

Das Erstellen einer temporären Datei kann in verschiedenen Situationen hilfreich sein. Manchmal müssen wir Daten für eine bestimmte Zeit speichern, aber danach werden sie nicht mehr benötigt. Oder wir benötigen eine Datei als Zwischenspeicher, um sie später zu bearbeiten und zu verwenden. In solchen Fällen ist das Erstellen einer temporären Datei eine schnelle und einfache Lösung.

## Wie man eine temporäre Datei erstellt
Die Erstellung einer temporären Datei in Elixir ist einfach und erfordert nur ein paar Zeilen Code. Wir verwenden dafür die Funktion `Tempfile.open/2`, wobei der erste Parameter der Dateiname ist und der zweite Parameter ein Funktionsaufruf ist, der den Dateiinhalt enthält. Schauen wir uns das in einem Beispiel an:

```Elixir
Tempfile.open("example.txt", fn(file) ->
  IO.write(file, "Dies ist ein Beispiel Text")
end)
```

Hier haben wir eine temporäre Datei mit dem Namen "example.txt" erstellt und den Inhalt "Dies ist ein Beispiel Text" in die Datei geschrieben. Die Datei wird automatisch geschlossen, sobald die Funktion ausgeführt wird.

Um zu überprüfen, ob die Datei erfolgreich erstellt wurde, können wir den folgenden Code verwenden:

```Elixir
File.regular?("example.txt")
# => true
```

Mit dieser einfachen Methode können wir temporäre Dateien in unseren Anwendungen erstellen und verwenden.

## Tieferer Einblick
Das Erstellen einer temporären Datei mit der `Tempfile.open/2` Funktion hat noch weitere Vorteile. Zum Beispiel kümmert sich die Funktion auch um die Vergabe eines eindeutigen Dateinamens, um Konflikte mit bereits existierenden Dateien zu vermeiden. Außerdem wird die temporäre Datei automatisch im "temp" Ordner des Betriebssystems erstellt, so dass wir uns keine Gedanken darüber machen müssen, wo wir die Datei speichern sollen.

Es ist auch wichtig zu beachten, dass die temporären Dateien automatisch gelöscht werden, sobald das Programm beendet wird oder die Dateiobjekte außerhalb des Scopes der Funktion `Tempfile.open/2` zerstört werden.

## Siehe auch
- [Elixir Dokumentation zu Tempfile](https://hexdocs.pm/elixir/Tempfile.html)
- [Blogpost über Dateioperationen in Elixir](https://codewithhugo.com/create-delete-edit-files-elixir/)
- [GitHub Repository mit Beispielen zur Arbeit mit Dateien in Elixir](https://github.com/sevenseacat/elixir-file-workshop)