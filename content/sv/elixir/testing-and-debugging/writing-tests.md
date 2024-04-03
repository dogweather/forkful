---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:46.961399-07:00
description: "Hur man g\xF6r: Elixir anv\xE4nder ExUnit som sitt inbyggda testramverk,\
  \ vilket \xE4r extremt kraftfullt och enkelt att anv\xE4nda. H\xE4r \xE4r ett grundl\xE4\
  ggande exempel:\u2026"
lastmod: '2024-03-13T22:44:37.570876-06:00'
model: gpt-4-0125-preview
summary: "Elixir anv\xE4nder ExUnit som sitt inbyggda testramverk, vilket \xE4r extremt\
  \ kraftfullt och enkelt att anv\xE4nda."
title: Skriva tester
weight: 36
---

## Hur man gör:
Elixir använder ExUnit som sitt inbyggda testramverk, vilket är extremt kraftfullt och enkelt att använda. Här är ett grundläggande exempel:

1. Skapa en ny testfil i `test`-mappen i ditt Elixir-projekt. Till exempel, om du testar en modul med namnet `MathOperations`, kan din testfil vara `test/math_operations_test.exs`.

```elixir
# test/math_operations_test.exs
defmodule MathOperationsTest do
  use ExUnit.Case

  # Det här är ett enkelt testfall för att kontrollera additions funktionen
  test "additionen av två tal" do
    assert MathOperations.add(1, 2) == 3
  end
end
```

För att köra dina tester, använd kommandot `mix test` i din terminal. Om funktionen `MathOperations.add/2` korrekt adderar två tal, kommer du att se utskrift liknande:

```
..

Avslutad på 0.03 sekunder
1 test, 0 fel
```

För tester som involverar externa tjänster eller API:er, kan du vilja använda mockbibliotek, såsom `mox`, för att undvika att träffa faktiska tjänster:

1. Lägg till `mox` i dina beroenden i `mix.exs`:

```elixir
defp deps do
  [
    {:mox, "~> 1.0.0", only: :test},
    # andra beroenden...
  ]
end
```

2. Definiera en mockmodul i din testhjälpare (`test/test_helper.exs`):

```elixir
Mox.defmock(HTTPClientMock, för: HTTPClientBehaviour)
```

3. Använd mocken i ditt testfall:

```elixir
# test/some_api_client_test.exs
defmodule SomeAPIClientTest do
  use ExUnit.Case
  import Mox

  # Det här säger till Mox att verifiera att den här mocken kallades som förväntat
  setup :verify_on_exit!

  test "hämtar data från API:et" do
    # Ställ in mocksvaret
    expect(HTTPClientMock, :get, fn _url -> {:ok, "Mockat svar"} end)
    
    assert SomeAPIClient.get_data() == "Mockat svar"
  end
end
```

När du kör `mix test`, gör den här inställningen att du kan isolera dina enhetstester från verkliga externa beroenden, och fokusera på beteendet hos din egen kod. Detta mönster säkerställer att dina tester körs snabbt och förblir tillförlitliga, oavsett externa tjänsters status eller internetanslutning.
