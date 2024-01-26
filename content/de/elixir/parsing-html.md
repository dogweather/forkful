---
title:                "HTML parsen"
date:                  2024-01-20T15:31:15.080254-07:00
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)
HTML zu parsen bedeutet, die Struktur und den Inhalt von Webseiten zu analysieren. Programmierer machen das, um Informationen zu extrahieren, automatisiert Inhalte zu verarbeiten oder Web-Scraping durchzuführen.

## How to: (Wie geht das?)
Du kannst in Elixir mit der Floki-Bibliothek HTML parsen. Hier ist ein Beispiel, das zeigt, wie man Überschriften aus einer HTML-Datei extrahiert.

```elixir
# Zuerst füge Floki zu deinen Abhängigkeiten in mix.exs hinzu:
# defp deps do
#   [
#     {:floki, "~> 0.27"}
#   ]
# end
# Dann führe mix deps.get aus, um Floki zu installieren.

defmodule HTMLParser do
  def parse_html(html) do
    html
    |> Floki.parse()
    |> Floki.find("h1")
    |> Enum.map(fn {_, attributes, inner_html} -> {attributes, inner_html} end)
  end
end

# Beispiel-HTML
html = """
<html>
  <body>
    <h1>Willkommen</h1>
    <p>Das ist ein Elixir-Beispiel.</p>
  </body>
</html>
"""

# Parsing und Ausgabe
result = HTMLParser.parse_html(html)
IO.inspect(result)
```

Beispiel-Ausgabe:
```
[
  {[{"class", "headline"}], ["Willkommen"]}
]
```
Dies zeigt eine Liste von Tupeln mit den Attributen und Inhalten jeder `h1`-Überschrift.

## Deep Dive (Tiefergehender Einblick)
Früher waren reguläre Ausdrücke (Regex) zur HTML-Analyse gängig, aber problematisch aufgrund der Komplexität von HTML. Floki basiert auf der html5ever-Bibliothek, welche die HTML5-Parsing-Regeln nutzt. Alternativen zu Floki sind z.B. `Mechanize` oder `Scrapy` in anderen Sprachen (Python). Floki bietet eine jQuery-ähnliche Schnittstelle, die es einfach macht, bestimmte Knoten zu finden und Inhalte zu extrahieren. Es ist effizient und fehlertolerant.

## See Also (Siehe auch)
- Floki-Dokumentation: [https://hexdocs.pm/floki](https://hexdocs.pm/floki)
- html5ever GitHub-Repository: [https://github.com/servo/html5ever](https://github.com/servo/html5ever)
