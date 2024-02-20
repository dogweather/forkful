---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:54.514226-07:00
description: "Att tolka HTML i Elixir inneb\xE4r att extrahera information fr\xE5\
  n HTML-dokument. Programmerare g\xF6r detta f\xF6r att programmatiskt interagera\
  \ med webbsidor,\u2026"
lastmod: 2024-02-19 22:04:56.810918
model: gpt-4-0125-preview
summary: "Att tolka HTML i Elixir inneb\xE4r att extrahera information fr\xE5n HTML-dokument.\
  \ Programmerare g\xF6r detta f\xF6r att programmatiskt interagera med webbsidor,\u2026"
title: Tolka HTML
---

{{< edit_this_page >}}

## Vad & Varför?

Att tolka HTML i Elixir innebär att extrahera information från HTML-dokument. Programmerare gör detta för att programmatiskt interagera med webbsidor, skrapa data eller automatisera webbinteraktioner, vilket möjliggör att applikationer dynamiskt kan förstå och utnyttja webbinnehåll.

## Hur man gör:

Elixir, med dess robusta modell för samtidighet och paradigmet för funktionell programmering, inkluderar inte inbyggd kapacitet för att tolka HTML. Du kan dock använda populära tredjepartsbibliotek som `Floki` för detta ändamål. Floki gör HTML-tolkning intuitiv och effektiv, genom att utnyttja Elixirs mönstermatchning och pipningsfunktioner.

Först, lägg till Floki i dina beroenden i mix.exs:

```elixir
defp deps do
  [
    {:floki, "~> 0.31.0"}
  ]
end
```

Kör sedan `mix deps.get` för att installera det nya beroendet.

Nu ska vi tolka en enkel HTML-sträng för att extrahera data. Vi kommer att leta efter titlar inuti `<h1>`-taggar:

```elixir
html_content = """
<html>
  <body>
    <h1>Hej, Elixir!</h1>
    <h1>En Annan Titel</h1>
  </body>
</html>
"""

titles = html_content
         |> Floki.find("h1")
         |> Floki.text()

IO.inspect(titles)
```

**Exempel på utdata:**

```elixir
["Hej, Elixir!", "En Annan Titel"]
```

För att gå djupare, säg att du vill extrahera länkar (`<a>`-taggar) tillsammans med deras href-attribut. Så här kan du uppnå det:

```elixir
html_content = """
<html>
  <body>
    <a href="https://elixir-lang.org/">Elixirs Officiella Webbplats</a>
    <a href="https://hexdocs.pm/">HexDocs</a>
  </body>
</html>
"""

links = html_content
        |> Floki.find("a")
        |> Enum.map(fn({_, attrs, [text]}) -> {text, List.keyfind(attrs, "href", 0)} end)

IO.inspect(links)
```

**Exempel på utdata:**

```elixir
[{"Elixirs Officiella Webbplats", {"href", "https://elixir-lang.org/"}}, {"HexDocs", {"href", "https://hexdocs.pm/"}}]
```

Detta tillvägagångssätt gör det möjligt för dig att navigera och tolka HTML-dokument effektivt, vilket gör uppgifter för extraktion och manipulation av webbdata enkla i Elixir-applikationer.
