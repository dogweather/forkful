---
title:                "Organisering av kode i funksjoner"
aliases:
- /no/elixir/organizing-code-into-functions/
date:                  2024-01-26T01:09:34.111248-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organisering av kode i funksjoner"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å organisere kode i funksjoner innebærer å gruppere relaterte operasjoner i gjenbrukbare blokker. Vi gjør dette for å forbedre lesbarhet og vedlikeholdbarhet, redusere duplisering, og forenkle testing.

## Hvordan:
La oss lage en enkel Elixir-funksjon for å gjøre ord store:

```elixir
defmodule StringUtils do
  def capitalize_words(setning) do
    setning
    |> String.split()
    |> Enum.map(&String.capitalize/1)
    |> Enum.join(" ")
  end
end

IO.puts StringUtils.capitalize_words("hei elixir verden")
```
Utdata:
```
Hei Elixir Verden
```
Her har vi pent pakket inn logikken for å gjøre ord store i en funksjon kalt `capitalize_words`.

## Dypdykk
I Elixir, og i det bredere Erlang VM-økosystemet, er funksjoner førsteklasses borgere, arver filosofien om å bryte ned problemer i mindre, håndterbare og isolerte deler. Historisk sett har denne funksjonelle tilnærmingen røtter i lambda kalkulus og Lisps, som fremmer filosofien om kode som data.

Alternativer for å organisere kode kan være å bruke makroer eller prosesser i Elixir for henholdsvis repeterende eller samtidige oppgaver. Når det gjelder implementering, kan Elixir-funksjoner håndtere mønstermatching og ta imot forskjellige argumenter (aritet), noe som gir dem allsidighet.

## Se Også
- [Elixirs offisielle dokumentasjon om funksjoner](https://hexdocs.pm/elixir/Kernel.html#functions)
- [Dave Thomas' "Programming Elixir"](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)
