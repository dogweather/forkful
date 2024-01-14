---
title:    "Elixir: Tests schreiben"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Warum
Unit-Tests sind eine effektive Möglichkeit, um sicherzustellen, dass der Code, den wir schreiben, fehlerfrei funktioniert. Indem wir Tests schreiben, können wir vermeiden, dass Fehler und Bugs in unseren Programmen auftreten. Dadurch sparen wir Zeit und Mühe bei der Fehlersuche und können uns auf die Entwicklung neuer Features konzentrieren.

## Wie 
Um mit Elixir Tests zu schreiben, müssen wir zunächst das Modul `ExUnit` importieren. In unserem Beispiel erstellen wir eine Funktion, die zwei Zahlen multipliziert und das Ergebnis zurückgibt.

```
Elixir import ExUnit.Case 
defmodule CalculatorTest do 
  use ExUnit.Case 
  test "multiplies two numbers" do 
    assert Calculator.multiply(2, 2) == 4 
  end 
end
```

In dem obigen Beispiel sehen wir, dass wir eine eigene Testklasse `CalculatorTest` erstellen und darin eine Testfunktion definieren. Diese Funktion verwendet die `assert` Methode, um das erwartete Ergebnis (hier gleich 4) mit dem tatsächlichen Ergebnis unserer `Calculator.multiply` Funktion zu vergleichen.

Wir können auch negative Tests schreiben, um sicherzustellen, dass unsere Funktionen auch in unerwarteten Situationen richtig funktionieren.

```
Elixir import ExUnit.Case 
defmodule CalculatorTest do 
  use ExUnit.Case 
  test "throws error for non-numeric input" do 
    assert_raise ArithmeticError, fn -> Calculator.multiply(2, "hello") end 
  end 
end
```

In diesem Beispiel erwarten wir, dass unsere Funktion `Calculator.multiply` einen Fehler wirft, wenn wir versuchen, sie mit einer Zeichenkette anstatt einer Zahl aufzurufen.

## Deep Dive
Unit-Tests können auch dazu beitragen, den Code besser zu strukturieren und zu dokumentieren. Indem wir uns bewusst mit den verschiedenen Funktionen und Eingaben auseinandersetzen, können wir unseren Code verbessern und ihn leichter warten.

Zudem ermöglicht Elixir es uns, Tests parallel auszuführen, was die Effizienz des Testprozesses erhöht. Auch die Integration von Tests in den Entwicklungsprozess ist sehr einfach und kann mithilfe von Continuous-Integration-Tools automatisiert werden.

## Siehe auch 
- [ExUnit Dokumentation](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Elixir Testing mit ExUnit](https://medium.com/@diamondgfx/solid-testing-with-elixir-1779b6b3f678)
- [Continuous-Integration-Tools für Elixir](https://medium.com/polynique/continuous-integration-for-elixir-phanthomjs-migrate-lint-test-1f427d21d41e)