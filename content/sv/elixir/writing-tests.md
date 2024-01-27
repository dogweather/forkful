---
title:                "Skriva tester"
date:                  2024-01-19
html_title:           "Arduino: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva tester är att koda små program som kollar att annan kod gör rätt saker. Programmerare testar för att snabbt upptäcka fel, förbättra kvaliteten och säkerställa att framtida ändringar inte bryter något.

## Hur gör man:

```elixir
defmodule ExampleTest do
  use ExUnit.Case
  doctest Example

  test "the truth" do
    assert 1 + 1 == 2
  end
end
```

Kör tester med: `mix test`

Förväntad output:

```
....

Finished in 0.03 seconds
1 tests, 0 failures
```

## Fördjupning

Testning i Elixir hanteras framförallt av ExUnit, ett inbyggt ramverk sedan Elixir 1.0. Alternativen inkluderar ESpec, som liknar RSpec från Ruby, och StreamData för property-based testing. ExUnit är tätt integrerat med språket och använder Elixirs makron för att förenkla testskrivning.

## Se också

- Elixir officiella dokumentation för ExUnit: https://hexdocs.pm/ex_unit/ExUnit.html
- En artikel om property-based testing med StreamData: https://elixir-lang.org/blog/2017/10/31/stream-data-property-based-testing-and-data-generation-for-elixir/
- ESpec på GitHub för en RSpec-liknande upplevelse: https://github.com/antonmi/espec
