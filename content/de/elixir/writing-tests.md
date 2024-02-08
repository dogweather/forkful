---
title:                "Tests Schreiben"
aliases:
- de/elixir/writing-tests.md
date:                  2024-02-03T19:30:18.808697-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tests Schreiben"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben von Tests in Elixir beinhaltet das Erstellen von automatisierten Skripten, um das Verhalten Ihres Codes zu validieren. Programmierer tun dies, um Qualität zu gewährleisten, Regressionen zu verhindern und das Refactoring von Code zu erleichtern. Dies macht den Entwicklungsprozess zuverlässiger und effizienter.

## Wie geht das:
Elixir verwendet ExUnit als sein eingebautes Testframework, welches extrem leistungsfähig und einfach zu verwenden ist. Hier ist ein einfaches Beispiel:

1. Erstellen Sie eine neue Testdatei im `test`-Verzeichnis Ihres Elixir-Projekts. Wenn Sie beispielsweise ein Modul namens `MathOperations` testen, könnte Ihre Testdatei `test/math_operations_test.exs` sein.

```elixir
# test/math_operations_test.exs
defmodule MathOperationsTest do
  use ExUnit.Case

  # Dies ist ein einfacher Testfall, um die Additionsfunktion zu überprüfen
  test "die Addition von zwei Zahlen" do
    assert MathOperations.add(1, 2) == 3
  end
end
```

Um Ihre Tests auszuführen, verwenden Sie den Befehl `mix test` in Ihrem Terminal. Wenn die Funktion `MathOperations.add/2` zwei Zahlen korrekt addiert, sehen Sie eine Ausgabe wie folgt:

```
..

Abgeschlossen in 0.03 Sekunden
1 Test, 0 Fehler
```

Für Tests, die externe Dienste oder APIs involvieren, möchten Sie vielleicht Mock-Bibliotheken wie `mox` verwenden, um echte Dienste zu vermeiden:

1. Fügen Sie `mox` Ihren Abhängigkeiten in `mix.exs` hinzu:

```elixir
defp deps do
  [
    {:mox, "~> 1.0.0", nur: :test},
    # andere Abhängigkeiten...
  ]
end
```

2. Definieren Sie ein Mock-Modul in Ihrem Testhelfer (`test/test_helper.exs`):

```elixir
Mox.defmock(HTTPClientMock, für: HTTPClientBehaviour)
```

3. Verwenden Sie den Mock in Ihrem Testfall:

```elixir
# test/some_api_client_test.exs
defmodule SomeAPIClientTest do
  use ExUnit.Case
  import Mox

  # Dies sagt Mox, dass dieser Mock wie erwartet aufgerufen wurde
  setup :verify_on_exit!

  test "erhält Daten von der API" do
    # Richten Sie die Mock-Antwort ein
    expect(HTTPClientMock, :get, fn _url -> {:ok, "Gefälschte Antwort"} end)
    
    assert SomeAPIClient.get_data() == "Gefälschte Antwort"
  end
end
```

Wenn Sie `mix test` ausführen, ermöglicht diese Einrichtung, dass Sie Ihre Unit-Tests von echten externen Abhängigkeiten isolieren, wobei der Fokus auf das Verhalten Ihres eigenen Codes liegt. Dieses Muster stellt sicher, dass Ihre Tests schnell laufen und zuverlässig bleiben, unabhängig vom Status externer Dienste oder der Internetverbindung.
