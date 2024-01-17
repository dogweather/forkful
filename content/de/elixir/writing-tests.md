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

## Was & Warum?
Schreiben von Tests ist eine gängige Praxis unter Programmierern, um sicherzustellen, dass der Code funktioniert wie erwartet. Tests sind spezielle Code-Fragmente, die überprüfen, ob bestimmte Teile des Codes die erwarteten Ergebnisse liefern. Es ist wichtig, Tests zu schreiben, um sicherzustellen, dass der Code zuverlässig, fehlerfrei und wartbar bleibt.

## Wie geht's?
Eine Testfunktion in Elixir wird mit dem ```defmodule``` und ```deftest``` Befehl definiert. Eine beliebte Testbibliothek ist ExUnit. Ein Beispiel für eine Testfunktion, die überprüft, ob die Funktion ```add``` korrekt zwei Zahlen addiert, sieht folgendermaßen aus:

```Elixir
defmodule MathTest do
  use ExUnit.Case

  test "addition should return the correct sum" do
    assert add(2, 3) == 5
  end

  ## Um die Tests auszuführen, geben Sie folgenden Befehl in die Elixir-Konsole ein:
  ## ExUnit.autorun()
end

```

Die Ausgabe sollte folgendermaßen aussehen:
```Elixir
1 test, 0 failures
```

## Tiefensuche
Das Schreiben von Tests ist Teil des Test Driven Development (TDD) Prozesses, bei dem Tests vor der Implementierung von Code geschrieben werden. Dadurch wird sichergestellt, dass der Code die erwarteten Ergebnisse liefert und ermöglicht es auch, Fehler früh im Entwicklungsprozess zu erkennen. Eine Alternative zu Elixir's ExUnit ist die Credo Testbibliothek.

## Mehr dazu
Weitere Informationen über das Schreiben von Tests in Elixir finden Sie in der offiziellen Dokumentation von ExUnit: https://hexdocs.pm/ex_unit/overview.html. Zusätzlich gibt es viele Tutorials und informative Blog-Beiträge online, die helfen können, das Konzept zu verstehen und zu implementieren.