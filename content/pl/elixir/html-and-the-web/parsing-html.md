---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:59.130046-07:00
description: "Parsowanie HTML w Elixirze polega na ekstrakcji informacji z dokument\xF3\
  w HTML. Programi\u015Bci robi\u0105 to, aby programowo wsp\xF3\u0142dzia\u0142a\u0107\
  \ ze stronami internetowymi,\u2026"
lastmod: '2024-03-13T22:44:35.039841-06:00'
model: gpt-4-0125-preview
summary: "Parsowanie HTML w Elixirze polega na ekstrakcji informacji z dokument\xF3\
  w HTML."
title: "Analiza sk\u0142adniowa HTML"
weight: 43
---

## Jak to zrobić:
Elixir, z jego rozbudowanym modelem współbieżności i paradygmatem programowania funkcyjnego, nie zawiera wbudowanych możliwości parsowania HTML. Można jednak użyć popularnych bibliotek stron trzecich, takich jak `Floki`, do tego celu. Floki czyni parsowanie HTML intuicyjnym i efektywnym, wykorzystując dopasowanie wzorców i rury (piping) w Elixirze.

Najpierw dodaj Floki do swoich zależności w mix.exs:

```elixir
defp deps do
  [
    {:floki, "~> 0.31.0"}
  ]
end
```

Następnie uruchom `mix deps.get`, aby zainstalować nową zależność.

Teraz przeanalizujmy prosty ciąg HTML, aby wydobyć dane. Poszukamy tytułów wewnątrz tagów `<h1>`:

```elixir
html_content = """
<html>
  <body>
    <h1>Witaj, Elixir!</h1>
    <h1>Inny Tytuł</h1>
  </body>
</html>
"""

titles = html_content
         |> Floki.find("h1")
         |> Floki.text()

IO.inspect(titles)
```

**Przykładowe wyjście:**

```elixir
["Witaj, Elixir!", "Inny Tytuł"]
```

Aby zagłębić się głębiej, powiedzmy, że chcesz wyodrębnić linki (tagi `<a>`) wraz z ich atrybutami href. Oto jak możesz to osiągnąć:

```elixir
html_content = """
<html>
  <body>
    <a href="https://elixir-lang.org/">Oficjalna strona Elixira</a>
    <a href="https://hexdocs.pm/">HexDocs</a>
  </body>
</html>
"""

links = html_content
        |> Floki.find("a")
        |> Enum.map(fn({_, attrs, [text]}) -> {text, List.keyfind(attrs, "href", 0)} end)
        
IO.inspect(links)
```

**Przykładowe wyjście:**

```elixir
[{"Oficjalna strona Elixira", {"href", "https://elixir-lang.org/"}}, {"HexDocs", {"href", "https://hexdocs.pm/"}}]
```

To podejście pozwala na efektywne nawigowanie i parsowanie dokumentów HTML, ułatwiając zadania ekstrakcji i manipulacji danymi internetowymi w aplikacjach Elixira.
