---
title:                "Elixir: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Warum

Es gibt viele Gründe, warum man in der Elixir-Programmierung prüfen möchte, ob ein Verzeichnis existiert. Zum Beispiel kann es sein, dass man eine Datei in einem bestimmten Verzeichnis erstellen möchte, aber sicherstellen möchte, dass das Verzeichnis auch tatsächlich existiert. Oder man möchte vermeiden, dass ein Programm abstürzt, wenn versucht wird, auf ein nicht vorhandenes Verzeichnis zuzugreifen. In diesem Blogbeitrag werde ich erklären, wie man in Elixir überprüfen kann, ob ein Verzeichnis vorhanden ist.

## Wie

In Elixir gibt es eine nützliche Funktion namens `File.exists?`, die verwendet werden kann, um zu überprüfen, ob eine bestimmte Datei oder ein Verzeichnis existiert. Diese Funktion gibt entweder `true` oder `false` zurück, abhängig davon, ob das angegebene Element vorhanden ist oder nicht.

Um zu überprüfen, ob ein Verzeichnis existiert, können wir einfach den Pfad zu dem Verzeichnis als Argument für die `File.exists?`-Funktion übergeben. Hier ist ein Beispiel:

```elixir
if File.exists?("/home/user/documents") do
  # Führe Code aus, der auf das Verzeichnis zugreift
else
  # Führe Alternativcode aus, falls das Verzeichnis nicht existiert
end
```

Wenn das Verzeichnis existiert, wird der Code innerhalb der `if`-Bedingung ausgeführt. Andernfalls wird der Code innerhalb des `else`-Blocks ausgeführt.

## Deep Dive

Es ist wichtig zu verstehen, dass die `File.exists?`-Funktion nur prüft, ob der angegebene Pfad existiert, unabhängig davon, ob es sich um ein Verzeichnis oder eine Datei handelt. Wenn Sie also sicherstellen möchten, dass es sich bei dem angegebenen Pfad um ein Verzeichnis handelt, können Sie die Funktion `File.dir?` verwenden. Diese Funktion gibt `true` zurück, wenn es sich bei dem angegebenen Pfad um ein Verzeichnis handelt, andernfalls gibt sie `false` zurück.

Eine gute Praxis ist es, die `File.dir?`-Funktion zuerst zu verwenden, um sicherzustellen, dass der angegebene Pfad tatsächlich ein Verzeichnis ist, bevor die `File.exists?`-Funktion aufgerufen wird.

## Siehe auch

- [Die offizielle Elixir-Dokumentation zur File-Module](https://hexdocs.pm/elixir/File.html)
- [Ein Elixir Weekly Blogbeitrag über das Arbeiten mit Dateien und Verzeichnissen](https://elixirweekly.net/issues/working-with-files-and-directories-248837)
- [Ein GitHub-Repository mit Beispielen zum Arbeiten mit Verzeichnissen in Elixir](https://github.com/alexfmsu/elixir_directory_examples)