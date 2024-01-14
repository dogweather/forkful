---
title:    "Elixir: Skrive tester"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elixir/writing-tests.md"
---

{{< edit_this_page >}}

# Hvorfor skrive enhetstester i Elixir?

Enhets- og funksjonstesting er viktige deler av å skrive god og pålitelig kode. Ved å skrive enhetstester i Elixir, kan du enkelt oppdage og fikse feil før de påvirker produksjonskoden din. Dette sparer deg for tid og frustrasjon senere, og gir deg en tryggere og mer effektiv utviklingsprosess.

# Hvordan skrive enhetstester i Elixir

Det finnes flere biblioteker og rammer for å skrive enhetstester i Elixir, men vi vil fokusere på Elixir's standard bibliotek `ExUnit`. La oss se på et enkelt eksempel:

```Elixir
defmodule Calculator do
  def add(a, b) do
    a + b
  end
end

defmodule CalculatorTest do
  use ExUnit.Case, async: true

  test "add/2 adds two numbers" do
    assert Calculator.add(2, 3) == 5
  end
end
```

Vi definerer en modul `Calculator` som inneholder en enkel funksjon for å legge sammen to tall. I testmodulen `CalculatorTest` bruker vi `ExUnit.Case` og definerer en test som sjekker om `Calculator.add(2, 3)` gir riktig resultat.

Vi kan kjøre testen ved å kjøre `mix test` i terminalen. Outputen vil være noe sånt som:

```
.

Finished in 0.03 seconds
1 test, 0 failures
```

Vi ser at testen vår passerte, siden det kun er én test og den er bestått. Dersom testen hadde feilet, ville vi fått en mer detaljert beskrivelse av feilen og hvor i koden den oppstår.

# Dykke dypere

Nå som vi har sett på et enkelt eksempel, kan vi dykke litt dypere inn i enhetstesting i Elixir. Noen ting du bør være oppmerksom på når du skriver enhetstester:

- Organiser testene dine i logiske `describe`- og `context`-blokker for bedre lesbarhet
- Bruk `setup` og `teardown` for å sette opp og rydde etter testene dine
- Bruk `assert` og `refute` for å sjekke om forventede verdier stemmer eller ikke
- `async: true` kan brukes for å få testene til å kjøre parallelt, men vær obs på eventuelle bugs dette kan føre til og test alltid med både `async: true` og `async: false`

For mer informasjon om enhetstesting i Elixir, sjekk ut følgende ressurser:

- [ExUnit dokumentasjon](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Elixir School: Testing](https://elixirschool.com/en/lessons/advanced/testing/)
- [Elixir Testing on Pluralsight](https://www.pluralsight.com/courses/elixir-testing)

# Se også

- [Elixir offisiell nettside](https://elixir-lang.org/)
- [Elixir på norsk](https://elixir.no/)
- [Elixir forum på norsk](https://forum.elixirforum.com/)