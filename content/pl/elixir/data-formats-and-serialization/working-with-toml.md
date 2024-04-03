---
date: 2024-01-26 04:20:58.323084-07:00
description: "Praca z TOML oznacza parsowanie i generowanie danych TOML (Tom's Obvious,\
  \ Minimal Language) za pomoc\u0105 Elixir. Programi\u015Bci u\u017Cywaj\u0105 go\
  \ do obs\u0142ugi plik\xF3w\u2026"
lastmod: '2024-03-13T22:44:35.068377-06:00'
model: gpt-4-0125-preview
summary: "Praca z TOML oznacza parsowanie i generowanie danych TOML (Tom's Obvious,\
  \ Minimal Language) za pomoc\u0105 Elixir."
title: Praca z TOML
weight: 39
---

## Jak to zrobić:
Najpierw, dodaj parser TOML do swoich zależności mix. Przykład używa `toml-elixir`:

```elixir
def deps do
  [
    {:toml_elixir, "~> 2.0"}
  ]
end
```

Czytanie pliku TOML:

```elixir
{:ok, toml_data} = File.read("config.toml")
{:ok, parsed_data} = TomlElixir.parse(toml_data)
```

Aby przekonwertować dane Elixir na TOML:

```elixir
data = %{title: "Przykład TOML", owner: %{name: "Tom Preston-Werner"}}
toml_string = TomlElixir.encode(data)
```

Przykładowy wynik:

```elixir
"title = \"Przykład TOML\"\n\n[owner]\nname = \"Tom Preston-Werner\"\n"
```

## Pogłębiona analiza
TOML został stworzony przez Toma Preston-Wernera, współzałożyciela GitHuba, do użytku w plikach konfiguracyjnych. Jest zaprojektowany, aby być bardziej prostolinijny niż XML i bardziej zwięzły niż YAML, jednocześnie utrzymując spójność.

Alternatywy obejmują JSON, YAML i pliki INI, każdy z własnymi kompromisami w czytelności dla ludzi i kompatybilności struktur danych. TOML wyróżnia się wyraźnym przedstawieniem danych tabelarycznych i zagnieżdżonym grupowaniem danych.

W Elixir, obsługa TOML zależy od bibliotek dekodowania i kodowania, które przekształcają ciągi TOML w mapy Elixir i odwrotnie. Parsowanie działa poprzez dopasowywanie do reguł składni TOML i konwertowanie ich na typy danych Elixir. Kodowanie robi to odwrotnie, mapując typy danych Elixir z powrotem na ważną składnię TOML.

## Zobacz również
- Język TOML: https://toml.io/en/
- Repozytorium GitHub `toml-elixir`: https://github.com/bitwalker/toml-elixir
- Szczegóły pakietu Hex dla `toml-elixir`: https://hex.pm/packages/toml_elixir
