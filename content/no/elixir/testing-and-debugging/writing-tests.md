---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:47.225591-07:00
description: "\xC5 skrive tester i Elixir involverer \xE5 lage automatiserte skript\
  \ for \xE5 validere oppf\xF8rselen til koden din. Programmerere gj\xF8r dette for\
  \ \xE5 sikre kvalitet,\u2026"
lastmod: '2024-03-13T22:44:40.446919-06:00'
model: gpt-4-0125-preview
summary: "\xC5 skrive tester i Elixir involverer \xE5 lage automatiserte skript for\
  \ \xE5 validere oppf\xF8rselen til koden din. Programmerere gj\xF8r dette for \xE5\
  \ sikre kvalitet,\u2026"
title: Skrive tester
weight: 36
---

## Hva & Hvorfor?
Å skrive tester i Elixir involverer å lage automatiserte skript for å validere oppførselen til koden din. Programmerere gjør dette for å sikre kvalitet, forhindre regresjoner, og lette koderestrukturering, noe som gjør utviklingsprosessen mer pålitelig og effektiv.

## Hvordan:
Elixir bruker ExUnit som sitt innebygde testrammeverk, som er ekstremt kraftfullt og enkelt å bruke. Her er et grunnleggende eksempel:

1. Lag en ny testfil i `test`-katalogen til Elixir-prosjektet ditt. For eksempel, hvis du tester en modul kalt `MathOperations`, kan testfilen din være `test/math_operations_test.exs`.

```elixir
# test/math_operations_test.exs
defmodule MathOperationsTest do
  use ExUnit.Case

  # Dette er et enkelt testtilfelle for å sjekke addisjonsfunksjonen
  test "addisjonen av to tall" do
    assert MathOperations.add(1, 2) == 3
  end
end
```

For å kjøre testene dine, bruk `mix test`-kommandoen i terminalen din. Hvis `MathOperations.add/2`-funksjonen korrekt legger til to tall, vil du se en utskrift som ligner på:

```
..

Ferdig på 0.03 sekunder
1 test, 0 feil
```

For tester som involverer eksterne tjenester eller APIer, kan du ønske å bruke mockbiblioteker, slik som `mox`, for å unngå å treffe faktiske tjenester:

1. Legg til `mox` i dine avhengigheter i `mix.exs`:

```elixir
defp deps do
  [
    {:mox, "~> 1.0.0", only: :test},
    # andre avhengigheter...
  ]
end
```

2. Definer en mockmodul i din testhjelper (`test/test_helper.exs`):

```elixir
Mox.defmock(HTTPClientMock, for: HTTPClientBehaviour)
```

3. Bruk mocken i testtilfellet ditt:

```elixir
# test/some_api_client_test.exs
defmodule SomeAPIClientTest do
  use ExUnit.Case
  import Mox

  # Dette forteller Mox å verifisere at denne mocken ble kalt som forventet
  setup :verify_on_exit!

  test "henter data fra APIet" do
    # Sett opp mockresponsen
    expect(HTTPClientMock, :get, fn _url -> {:ok, "Mocket respons"} end)
    
    assert SomeAPIClient.get_data() == "Mocket respons"
  end
end
```

Når du kjører `mix test`, lar denne oppsettet deg isolere enhetstestene dine fra ekte eksterne avhengigheter, og fokuserer på oppførselen til din egen kode. Dette mønsteret sikrer at testene dine kjører raskt og forblir pålitelige, uavhengig av ekstern tjenestestatus eller internettforbindelse.
