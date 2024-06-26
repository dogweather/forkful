---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:25.279462-07:00
description: "Hvordan: Elixir, med sin robuste samtidighetsmodell og funksjonelle\
  \ programmeringsparadigme, inkluderer ikke innebygde HTML-parsingmuligheter. Du\
  \ kan\u2026"
lastmod: '2024-03-13T22:44:40.441375-06:00'
model: gpt-4-0125-preview
summary: Elixir, med sin robuste samtidighetsmodell og funksjonelle programmeringsparadigme,
  inkluderer ikke innebygde HTML-parsingmuligheter.
title: Analysering av HTML
weight: 43
---

## Hvordan:
Elixir, med sin robuste samtidighetsmodell og funksjonelle programmeringsparadigme, inkluderer ikke innebygde HTML-parsingmuligheter. Du kan imidlertid bruke populære tredjepartsbiblioteker som `Floki` for dette formålet. Floki gjør HTML-parsing intuitiv og effektiv ved å utnytte Elixirs mønstermatching og piping-funksjoner.

Først, legg til Floki i dine mix.exs-avhengigheter:

```elixir
defp deps do
  [
    {:floki, "~> 0.31.0"}
  ]
end
```

Deretter kjører du `mix deps.get` for å installere den nye avhengigheten.

Nå skal vi parse en enkel HTML-streng for å trekke ut data. Vi ser etter titlene inne i `<h1>`-taggene:

```elixir
html_content = """
<html>
  <body>
    <h1>Hei, Elixir!</h1>
    <h1>En annen tittel</h1>
  </body>
</html>
"""

titles = html_content
         |> Floki.find("h1")
         |> Floki.text()

IO.inspect(titles)
```

**Eksempelutskrift:**

```elixir
["Hei, Elixir!", "En annen tittel"]
```

For å gå dypere inn, si at du vil trekke ut lenker (`<a>`-tagger) sammen med deres href-attributter. Her er hvordan du kan oppnå det:

```elixir
html_content = """
<html>
  <body>
    <a href="https://elixir-lang.org/">Elixirs offisielle nettside</a>
    <a href="https://hexdocs.pm/">HexDocs</a>
  </body>
</html>
"""

links = html_content
        |> Floki.find("a")
        |> Enum.map(fn({_, attrs, [text]}) -> {text, List.keyfind(attrs, "href", 0)} end)
        
IO.inspect(links)
```

**Eksempelutskrift:**

```elixir
[{"Elixirs offisielle nettside", {"href", "https://elixir-lang.org/"}}, {"HexDocs", {"href", "https://hexdocs.pm/"}}]
```

Denne tilnærmingen lar deg navigere og parse HTML-dokumenter effektivt, noe som gjør oppgaver knyttet til ekstraksjon og manipulasjon av webdata rettfram i Elixir-applikasjoner.
