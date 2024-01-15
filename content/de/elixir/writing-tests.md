---
title:                "Tests schreiben"
html_title:           "Elixir: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Tests zu schreiben mag auf den ersten Blick wie eine zusätzliche und möglicherweise langweilige Aufgabe erscheinen, die Sie wegen Zeitmangel lieber überspringen möchten. Aber in Wirklichkeit kann das Schreiben von Tests Ihnen viel Zeit und Frustration ersparen, indem es hilft, Fehler in Ihrem Code frühzeitig zu erkennen und zu verhindern. Außerdem ermöglichen Tests auch das Refactoring und die Weiterentwicklung Ihrer Anwendung mit mehr Vertrauen.

## Wie geht es

Um Tests in Elixir zu schreiben, verwenden Sie die eingebaute Test-Bibliothek ExUnit. Beginnen wir mit einem einfachen Beispiel eines Tests, der überprüft, ob eine Funktion den richtigen Wert zurückgibt:

```Elixir
defmodule Math do
  def add(a, b) do
    a + b
  end
end

defmodule MathTest do
  use ExUnit.Case

  test "adding numbers" do
    assert Math.add(2, 3) == 5
  end
end
```

Die `defmodule`-Anweisung definiert ein Modul, in dem wir unsere Funktion `add/2` definieren. In unserem Testmodul verwenden wir `ExUnit.Case`, um den Testfall zu definieren und mit `assert` überprüfen wir, ob unsere Funktion `Math.add/2` den erwarteten Wert von 5 zurückgibt.

Sie können dieses Beispiel ausführen, indem Sie in Ihrem Terminal `mix test` eingeben. Sie sollten ein Ergebnis sehen, das besagt, dass alle Tests erfolgreich waren:

```bash
Finished in 0.02 seconds
1 test, 0 failures
```

Sie können auch Parameter an `mix test` übergeben, um bestimmte Tests auszuführen. Weitere Informationen zu ExUnit und seinen Funktionen finden Sie in der offiziellen Dokumentation.

## Deep Dive

Tests können auf verschiedene Arten geschrieben werden, aber im Allgemeinen gibt es zwei Hauptansätze: Ein Test kann entweder die erwarteten Ergebnisse des Codes überprüfen oder überprüfen, ob der Code die gewünschte Funktion ausführt. Mit der `assert`-Funktion können Sie die erwarteten Ergebnisse überprüfen, während die `refute`-Funktion verwendet wird, um sicherzustellen, dass ein bestimmter Wert nicht zurückgegeben wird.

Es ist auch wichtig zu beachten, dass Tests nicht nur dazu dienen, Ihren Code auf Fehler zu überprüfen, sondern auch dazu, Ihre Anwendung zu dokumentieren und anderen Entwicklern zu zeigen, wie Sie sie verwenden können. Eine gut geschriebene Test-Suite kann auch als Teil der Dokumentation Ihrer Anwendung dienen.

## Siehe auch

- Offizielle Elixir-Website (https://elixir-lang.org/)
- ExUnit-Dokumentation (https://hexdocs.pm/ex_unit/ExUnit.html)
- Pragmatic Programmer: "Elixir & Phoenix - a modern approach to dynamic software development" (https://pragprog.com/book/phoenix12/programming-elixir-1-2)