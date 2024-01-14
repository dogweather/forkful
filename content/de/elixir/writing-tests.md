---
title:                "Elixir: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Warum?

Tests sind ein unerlässlicher Bestandteil der Softwareentwicklung und können dabei helfen, Fehler frühzeitig zu erkennen und die Qualität des Codes zu verbessern. Durch das Schreiben von Tests kann man sicherstellen, dass die Funktionen eines Programms wie erwartet arbeiten und dass Änderungen keine unerwünschten Nebenwirkungen haben. In diesem Blogpost werden wir uns anschauen, wie man Tests in Elixir schreibt und warum es wichtig ist, dies in seinen Entwicklungsprozess einzubinden.

## Wie funktionieren Tests in Elixir?

In Elixir gibt es das eingebaute Testing-Framework ExUnit, welches uns beim Schreiben von Tests unterstützt. Zunächst müssen wir unsere Testdateien im Ordner "test" speichern und mit der Erweiterung ".exs" versehen. Um eine Testdatei zu erstellen, können wir entweder das Kommandozeilen-Tool "mix" verwenden oder wir können sie manuell erstellen.

Das Grundgerüst einer Testdatei in Elixir sieht wie folgt aus:

```Elixir
defmodule Test do
  use ExUnit.Case

  test "beispiel test" do
    assert 1 + 1 == 2
  end
end
```

Hier haben wir eine Testklasse erstellt und eine Testfunktion hinzugefügt, in der wir unsere Assertion schreiben. Mit `assert` vergleichen wir, ob der Ausdruck auf der linken Seite gleich dem auf der rechten Seite ist. Wenn dies der Fall ist, wird der Test als erfolgreich angesehen. Andernfalls wird er als fehlgeschlagen markiert.

Um unsere Tests auszuführen, können wir das Kommando `mix test` verwenden. Dabei werden alle Testdateien im "test"-Ordner ausgeführt und die Ergebnisse werden in der Konsole angezeigt.

## Tieferer Einblick in Teststrukturen

ExUnit bietet noch viele weitere Möglichkeiten, um unsere Tests zu strukturieren und zu organisieren. Zum Beispiel können wir unsere Tests in Kontexte gruppieren, um einzelne Teile unserer Anwendung zu testen. Wir können auch Setup- und Teardown-Funktionen verwenden, um vor und nach jedem Test bestimmte Aktionen auszuführen. Es gibt auch die Möglichkeit, Testdaten zu generieren und Mocks zu verwenden, um komplexere Testszenarien abzudecken.

Es ist wichtig, sich mit den verschiedenen Funktionen und Möglichkeiten von ExUnit vertraut zu machen, um effektive und robuste Tests zu schreiben.

## Siehe auch

- Offizielle Dokumentation zu ExUnit: https://hexdocs.pm/ex_unit/ExUnit.html
- "Why Testing Matters" von José Valim: https://blog.plataformatec.com.br/2015/10/why-testing-matters/
- "Unit Testing in Elixir with ExUnit" von Gábor Szabó: https://pragprog.com/book/lmelixir/unit-testing-with-elixir