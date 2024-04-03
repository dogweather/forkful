---
date: 2024-01-26 04:39:21.252207-07:00
description: "Komplekse tall har en reell del og en imagin\xE6r del (som `3 + 4i`).\
  \ De brukes i ingeni\xF8rfag, fysikk og visse databehandlingsproblemer. Programmerere\u2026"
lastmod: '2024-03-13T22:44:40.437340-06:00'
model: gpt-4-0125-preview
summary: "Komplekse tall har en reell del og en imagin\xE6r del (som `3 + 4i`)."
title: "\xC5 jobbe med komplekse tall"
weight: 14
---

## Hva & Hvorfor?
Komplekse tall har en reell del og en imaginær del (som `3 + 4i`). De brukes i ingeniørfag, fysikk og visse databehandlingsproblemer. Programmerere arbeider med dem for simuleringer, signalbehandling og løsning av visse typer matteproblemer effektivt.

## Hvordan:
Elixir har ikke innebygde komplekse tall, så vi lager våre egne eller bruker et bibliotek, som `ComplexNum`. Her er et kjapt eksempel med et bibliotek:

```elixir
# Antatt at du har installert ComplexNum
defmodule ComplexMath do
  import ComplexNum

  def add(a, b) do
    ComplexNum.add(a, b)
  end
end

# Skap komplekse tall og legg dem sammen
c1 = {3, 4}   # representerer 3 + 4i
c2 = {2, -3}  # representerer 2 - 3i
resultat = ComplexMath.add(c1, c2)
IO.puts "Resultatet er: #{inspect(resultat)}"
```

Dette ville gi utskriften:
```
Resultatet er: {5, 1}
```

Det betyr at summen av `3 + 4i` og `2 - 3i` er `5 + 1i`.

## Dypdykk
Komplekse tall dukket opp i historien fordi vanlige gamle tall ikke kunne håndtere kvadratrøtter av negative tall. Det var ikke før på 1600-tallet at de ble tatt seriøst, takket være matematikere som René Descartes og Gerolamo Cardano.

I Elixir bruker du ofte tupler som `{3, 4}` for komplekse tall, eller bruker et dedikert bibliotek for å unngå å finne opp hjulet på nytt. Biblioteker er vanligvis bedre - de håndterer det nitidige som multiplikasjon og divisjon, som blir komplisert på grunn av den imaginære enheten 'i' (BTW: `i` i andre er lik `-1`).

## Se også
Sjekk ut disse ressursene:
- [ComplexNum Library](https://hex.pm/packages/complex_num) for Elixir sitt pakkehåndteringssystem, Hex.
- [Elixir School](https://elixirschool.com/en/), for avanserte Elixir emner og øvelser.
- [Erlang -- math Module](http://erlang.org/doc/man/math.html), som Elixir bruker under panseret, for andre matematiske behov.
