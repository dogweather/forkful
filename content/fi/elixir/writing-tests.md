---
title:                "Testien kirjoittaminen"
date:                  2024-01-19
html_title:           "Arduino: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"

category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Testaus varmistaa koodin toiminnan. Se vähentää bugeja ja parantaa ohjelmiston laatua.

## How to:
Elixirissä testit kirjoitetaan ExUnitilla, oletustestikehyksellä.

```elixir
# test/example_test.exs
defmodule ExampleTest do
  use ExUnit.Case
  doctest Example

  test "summa funktio" do
    assert Example.sum(1, 2) == 3
  end
end
```

Aja testit komennolla:

```shell
mix test
```

Testien tuloksen pitäisi olla:

```
..

Finished in 0.03 seconds
1 test, 0 failures
```

## Deep Dive:
ExUnit on Elixiriin sisältyvä testimoduuli, esitelty kielen ensimmäisissä versioissa. Vaihtoehtoisia testityökaluja on harvassa, mutta jotkut devaajat käyttävät property-based testingiä kirjaston `StreamData` kanssa. ExUnit toiminta perustuu makroihin, jotka luovat moduuleita ja funktioita kulissien takana.

## See Also:
- Elixirin virallinen dokumentaatio: [https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html#exunit](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html#exunit)
- Elixir School, testaus: [https://elixirschool.com/en/lessons/basics/testing/](https://elixirschool.com/en/lessons/basics/testing/)
- Elixiristä kiinnostuneille: "Programming Elixir" kirja.
