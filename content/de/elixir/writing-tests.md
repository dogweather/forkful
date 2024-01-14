---
title:    "Elixir: Tests schreiben"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Warum?

Tests sind ein unverzichtbarer Teil der Elixir Programmierung. Sie helfen dabei, Bugs zu vermeiden und ein stabiles und zuverlässiges Programm zu erstellen. Durch das Schreiben von Tests können Entwickler*innen sicherstellen, dass ihr Code korrekt funktioniert und alle Anforderungen erfüllt werden. Außerdem erleichtern Tests die Fehlerbehebung, da sie helfen, den Ursprung von Problemen schneller zu finden.

## Wie man Tests schreibt

Die Erstellung von Tests in Elixir ist recht einfach. Zunächst muss man [`ExUnit`](https://hexdocs.pm/ex_unit/ExUnit.html) importieren, das das Testen in Elixir ermöglicht. Dann kann man mit der `@tag :test` Annotation Funktionen als Tests markieren. Innerhalb dieser Funktionen können verschiedene [`assert`](https://hexdocs.pm/ex_unit/ExUnit.Assertions.html#assert/2) Ansprüche verwendet werden, um das erwartete Verhalten des Codes zu überprüfen.

```Elixir
defmodule MyModuleTest do
  use ExUnit.Case

  @tag :test
  def test_my_function do
    assert 1 + 1 == 2
    assert String.length("Hello") == 5
  end
end
```

## Tiefer Einblick

Es gibt verschiedene Arten von Tests, die in Elixir geschrieben werden können, darunter Unit-Tests, Integrationstests und Funktionstests. Unit-Tests überprüfen einzelne Funktionen oder Module, während Integrationstests das Zusammenspiel von mehreren Komponenten testen. Funktionstests hingegen simulieren tatsächliches Benutzerverhalten und testen die Anwendung von außen.

Beim Schreiben von Tests ist es wichtig, auch unerwartete Fälle zu berücksichtigen und Randbedingungen zu testen. Hier können auch sogenannte [`property tests`](https://hexdocs.pm/stream_data/ExUnitProperties.html) helfen, die zufällige Werte generieren und so mögliche Bugs aufdecken können.

Siehe auch

- [Elixir School: Testing](https://elixirschool.com/en/lessons/basics/testing/)
- [Elixir Testing Tutorial](https://www.tutorialspoint.com/elixir/elixir_testing.htm)
- [ExUnit Documentation](https://hexdocs.pm/ex_unit/ExUnit.html)