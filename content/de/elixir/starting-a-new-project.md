---
title:                "Elixir: Ein neues Projekt beginnen."
programming_language: "Elixir"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum man ein neues Elixir-Projekt starten möchte. Vielleicht ist es ein persönliches Projekt, an dem man schon lange arbeitet und endlich in Produktionsumgebung bringen möchte. Oder vielleicht ist es ein geschäftliches Projekt, das von Grund auf mit Elixir aufgebaut werden soll. Egal aus welchem Grund, in diesem Blogbeitrag werden wir uns anschauen, wie man ein neues Elixir-Projekt startet und was dabei zu beachten ist.

## Wie geht's?

Um ein neues Elixir-Projekt zu starten, müssen wir zunächst die Elixir-Entwicklungsumgebung einrichten. Dazu benötigen wir die Elixir-Sprache und die dazugehörige Laufzeitumgebung. Wenn du noch keine Erfahrung mit Elixir hast, ist es empfehlenswert, die offizielle Dokumentation zu lesen und ein paar Tutorials zu durchlaufen, um dich mit der Sprache vertraut zu machen.

Nachdem wir die Entwicklungsumgebung eingerichtet haben, können wir ein neues Projekt erstellen, indem wir das Terminal/ die Konsole öffnen und folgenden Befehl ausführen:

```Elixir
mix new my_project
```

Dieser Befehl erstellt ein neues Elixir-Projekt mit dem Namen "my_project" und fügt alle benötigten Dateien und Ordner hinzu. Nun können wir in unserem Projektordner die Datei "mix.exs" öffnen und die gewünschten Abhängigkeiten hinzufügen, indem wir sie in der `deps` Funktion angeben.

```Elixir
defp deps do
  [
    {:phoenix, "~> 1.4.0"}
  ]
end
```

Anschließend müssen wir die Abhängigkeiten mit dem Befehl `mix deps.get` installieren. Danach können wir unseren Code schreiben und mit `mix compile` kompilieren. Um unser Projekt auszuführen, können wir entweder den Befehl `mix run` verwenden, um alle Dateien auszuführen, oder wir können das Modul direkt aus dem Terminal aufrufen:

```Elixir
iex -S mix
```

Jetzt können wir mit unserem neuen Elixir-Projekt loslegen!

## Tieferer Einblick

Bevor wir mit der Entwicklung unseres Projekts beginnen, gibt es noch ein paar wichtige Dinge zu beachten. Elixir verwendet das Konzept von "Supervisors" und "Workers", um Fehler zu verwalten und die Stabilität unserer Anwendung zu gewährleisten. Deshalb ist es wichtig, sich mit diesem Konzept vertraut zu machen und die richtige Strukturierung unserer Anwendung zu berücksichtigen.

Außerdem bietet Elixir eine leistungsfähige Standardbibliothek mit vielen nützlichen Funktionen und Modulen. Es lohnt sich, sich mit diesen vertraut zu machen und sie in unserem Projekt einzusetzen, um effizientere und wartbarere Code zu schreiben.

## Siehe auch

- Offizielle Elixir-Dokumentation: https://elixir-lang.org
- Elixir-Tutorials: https://elixir-lang.org/learning.html
- Elixir Standardbibliothek: https://hexdocs.pm/elixir/Kernel.html