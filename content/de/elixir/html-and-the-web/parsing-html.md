---
title:                "HTML parsen"
aliases:
- /de/elixir/parsing-html.md
date:                  2024-02-03T19:11:42.251444-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML parsen"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

HTML in Elixir zu parsen, umfasst das Extrahieren von Informationen aus HTML-Dokumenten. Programmierer tun dies, um programmatisch mit Webseiten zu interagieren, Daten zu scrapen oder Webinteraktionen zu automatisieren, wodurch Anwendungen Webinhalte dynamisch verstehen und nutzen können.

## Wie geht das:

Elixir, mit seinem robusten Nebenläufigkeitsmodell und dem Paradigma der funktionalen Programmierung, enthält keine integrierten HTML-Parsing-Funktionen. Sie können jedoch beliebte Drittanbieter-Bibliotheken wie `Floki` für diesen Zweck verwenden. Floki macht HTML-Parsing intuitiv und effizient und nutzt dabei Elixirs Musterabgleich und Pipe-Funktionen.

Fügen Sie zunächst Floki zu Ihren Abhängigkeiten in mix.exs hinzu:

```elixir
defp deps do
  [
    {:floki, "~> 0.31.0"}
  ]
end
```

Führen Sie dann `mix deps.get` aus, um die neue Abhängigkeit zu installieren.

Jetzt werden wir einen einfachen HTML-String parsen, um Daten zu extrahieren. Wir suchen nach den Titeln in `<h1>`-Tags:

```elixir
html_content = """
<html>
  <body>
    <h1>Hallo, Elixir!</h1>
    <h1>Ein weiterer Titel</h1>
  </body>
</html>
"""

titles = html_content
         |> Floki.find("h1")
         |> Floki.text()

IO.inspect(titles)
```

**Beispielausgabe:**

```elixir
["Hallo, Elixir!", "Ein weiterer Titel"]
```

Um tiefer einzutauchen, sagen wir, Sie möchten Links (`<a>`-Tags) zusammen mit ihren href-Attributen extrahieren. So können Sie das erreichen:

```elixir
html_content = """
<html>
  <body>
    <a href="https://elixir-lang.org/">Offizielle Website von Elixir</a>
    <a href="https://hexdocs.pm/">HexDocs</a>
  </body>
</html>
"""

links = html_content
        |> Floki.find("a")
        |> Enum.map(fn({_, attrs, [text]}) -> {text, List.keyfind(attrs, "href", 0)} end)
        
IO.inspect(links)
```

**Beispielausgabe:**

```elixir
[{"Offizielle Website von Elixir", {"href", "https://elixir-lang.org/"}}, {"HexDocs", {"href", "https://hexdocs.pm/"}}]
```

Dieser Ansatz ermöglicht es Ihnen, HTML-Dokumente effizient zu navigieren und zu parsen, wodurch Aufgaben der Webdatenextraktion und -manipulation in Elixir-Anwendungen unkompliziert werden.
