---
date: 2024-01-26 01:09:34.111248-07:00
description: "Hvordan: La oss lage en enkel Elixir-funksjon for \xE5 gj\xF8re ord\
  \ store."
lastmod: '2024-03-13T22:44:40.448772-06:00'
model: gpt-4-1106-preview
summary: "La oss lage en enkel Elixir-funksjon for \xE5 gj\xF8re ord store."
title: Organisering av kode i funksjoner
weight: 18
---

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
