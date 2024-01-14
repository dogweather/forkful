---
title:                "Elixir: Utskrift av feilsøkingsutdata"
programming_language: "Elixir"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Alle utviklere støter på problemer som trenger debugging på et tidspunkt i sin karriere. Det kan være vanskelig å finne ut hva som forårsaker et program å bryte eller å skrive kode som gir forventet resultat. En måte å hjelpe til å feilsøke dine Elixir-programmer er å utnytte debug output. Det kan gi verdifull informasjon som kan hjelpe deg å identifisere og fikse bugs.

## Slik gjør du det

For å skrive debug output i Elixir, bruker du funksjonen "IO.inspect". Dette vil skrive ut til konsollen i ditt utviklingsmiljø, som for eksempel Elixir REPL (read-eval-print loop) eller terminalen hvis du kjører din Elixir-applikasjon som en CLI-applikasjon.

```Elixir
defmodule User do
  def display_name(user) do
    IO.inspect user
    "Brukernavn: " <> user.name
  end
end

user = %{
  name: "Jane Doe",
  age: 30
}

User.display_name(user)
```

Output:

```
%{age: 30, name: "Jane Doe"}
Brukernavn: Jane Doe
```

Du kan også bruke IO.inspect når du er midt i en funksjon for å se på verdier av variabler eller argumenter. Dette kan være nyttig hvis du prøver å finne ut hvorfor en bestemt del av koden gir uventet resultat.

## Dypdykk

Det er verdt å nevne at hvis koden din er inne i en try/catch-blokk, vil IO.inspect ikke virke som forventet. Dette skyldes at IO.inspect skriver til standard utgangen, som i dette tilfellet vil være feilmeldingen. Derfor er det bedre å bruke Logger-funksjonen "debug" for å skrive ut i dette tilfellet.

```Elixir
try do
  nome = "katten"
  IO.inspect nome
  throw(:an_error)
catch
  _:throw ->
    Logger.debug "Variablen 'navn' er ikke tilgjengelig her."
end
```

Output i loggen:

```
[debug] Variablen 'nome' er ikke tilgjengelig her.
```

Det er også mulig å bruke IO.inspect med flere argumenter og til og med tilpasse utseendet på output ved hjelp av formateringsvalg. For mer informasjon, se Elixir dokumentasjonen for IO.inspect funksjonen.

## Se også

- [Elixir dokumentasjon - IO.inspect](https://hexdocs.pm/elixir/IO.html#inspect/2)
- [Elixir dokumentasjon - Logger](https://hexdocs.pm/logger/Logger.html)