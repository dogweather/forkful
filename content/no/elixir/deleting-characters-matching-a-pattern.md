---
title:                "Slette tegn som samsvarer med et mønster"
html_title:           "Arduino: Slette tegn som samsvarer med et mønster"
simple_title:         "Slette tegn som samsvarer med et mønster"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å slette tegn som matcher et mønster, vil si å fjerne spesifikke tegn fra en tekststreng basert på et gitt mønster. Dette gjør programmerere for å manipulere data, hjelpe med formatert utgang, og forandre tekst basert på spesifikke krav.

## Slik gjør du:
Her er et kodeeksempel på hvordan du kan slette tegn som matcher et mønster i Elixir:

```Elixir
defmodule Teksthåndtering do
  def slett_matchende_tegn(tekst, mønster) do
    Regex.replace(~r/#{mønster}/, tekst, "")
  end
end

IO.puts Teksthåndtering.slett_matchende_tegn("Hello, Verden!", "[, !]")
```

Dette gir utgangen:

```Elixir
"HelloVerden"
```

## Dypdykk
Denne teksthåndteringsfunksjonen i Elixir drar fordel av Erlang VM, som Elixir kjører på toppen av. Historisk brukte programmerere vanlig uttrykk eller "regex" i Perl for å matche og manipulere tekst. Elixir gir oss en mer moderne og effektiv måte å håndtere dette på.

Alternativt kan programmerere bruke funksjonen `String.replace/3` i stedet for `Regex.replace/3`. Men, en enkelt `String.replace/3` kan kun erstatte eller slette et enkelt, bestemt tegn, og ikke et mønster av tegn.

Hvis det som skal slettes er veldig komplekst, kan det hende du må lage en mer sofistikert regex, eller til og med bruke en ekstern tjeneste. Husk at bruken av komplekse regex-uttrykk kan redusere applikasjonens ytelse.

## Se også 
- For mer info om `Regex.replace/3`, se [Elixir Regex dok](https://hexdocs.pm/elixir/Regex.html#replace/3).
- For mer info om `String.replace/3`, se [Elixir String dok](https://hexdocs.pm/elixir/String.html#replace/3).