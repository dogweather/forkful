---
title:                "Elixir: Tests schreiben"
programming_language: "Elixir"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

In der Welt des Programmierens gibt es oft den Mythos, dass Tests eine lästige und zeitaufwendige Aufgabe sind, die keinen wirklichen Nutzen bringt. Doch tatsächlich sind Tests ein unverzichtbarer Bestandteil der Entwicklung eines zuverlässigen und fehlerfreien Codes. In diesem Blog-Beitrag werden wir uns genauer ansehen, warum das Schreiben von Tests in Elixir so wichtig ist.

## Wie geht das?

Die Erstellung von Tests in Elixir ist einfach und kann mithilfe des `ExUnit`-Frameworks durchgeführt werden. Um einen Test zu schreiben, müssen wir zuerst unsere Funktion definieren, die wir testen möchten. Nehmen wir zum Beispiel diese Funktion, die die ersten beiden Elemente einer Liste zurückgibt:

```Elixir
defmodule Math do
  def first_two(list) do
    Enum.take(list, 2)
  end
end
```

Um diesen Code zu testen, können wir Folgendes tun:

```Elixir
defmodule MathTest do
  use ExUnit.Case

  test "returns the first two elements of a list" do
    assert Math.first_two([1, 2, 3]) == [1, 2]
  end
end
```

Wenn wir nun den Test ausführen, werden wir eine erfolgreiche Ausgabe sehen, da unsere Funktion wie erwartet funktioniert:

```
$ mix test

Compiling 2 files (.ex)
..

Finished in 0.1 seconds
2 tests, 0 failures
```

Ein weiteres nützliches Feature von `ExUnit` ist die Möglichkeit, bestimmte Fälle zu überprüfen, z. B. das Handling von Fehlern. Wir können dies tun, indem wir assertions verwenden, die auf Fehlern prüfen, wie z. B. `assert_raise`:

```Elixir
test "returns an error if list is empty" do
  assert_raise FunctionClauseError, fn -> Math.first_two([]) end
end
```

Nun, da wir wissen, wie man Tests schreibt, lassen Sie uns einen genaueren Blick darauf werfen, warum es so wichtig ist, dies zu tun.

## Tiefes Eintauchen

Tests sind aus mehreren Gründen wichtig. Zunächst ermöglichen sie uns, die Qualität unseres Codes zu gewährleisten, indem sie seine Funktionalität überprüfen. Indem wir einzelne Funktionen testen, können wir Fehler und Lücken in unserem Code aufdecken, bevor sie zu Problemen führen.

Tests helfen auch dabei, Vertrauen in unseren Code aufzubauen. Wenn wir wissen, dass unsere Tests erfolgreich sind, können wir sicher sein, dass unser Code wie erwartet funktioniert und dass wir Änderungen vornehmen können, ohne dass der Code instabil wird.

Darüber hinaus sind Tests unerlässlich, um die Skalierbarkeit unseres Codes zu gewährleisten. Da Elixir eine funktionale Programmiersprache ist, kann unser Code problemlos auf mehreren Prozessoren und Rechnern ausgeführt werden. Durch das Schreiben von Tests können wir überprüfen, ob unser Code auch bei höherer Last einwandfrei funktioniert.

Schließlich helfen Tests dabei, unseren Code zu dokumentieren und zu verstehen. Indem wir Tests schreiben, veranschaulichen wir, wie unser Code verwendet werden sollte und können dies als Referenz für zukünftige Entwickler verwenden.

## Siehe auch

- [Offizielle Elixir-Dokumentation zu Testen](https://hexdocs.pm/elixir/ex_unit.html)
- [Übersicht über Testen in Elixir von thoughtbot](https://thoughtbot.com/blog/elixir-for-programmers-testing)
- [Einführung in Testen in Elixir von Andrea Leopardi](https://andrearichiardi.com/blog/posts/test-driven-development-with-elixir/)