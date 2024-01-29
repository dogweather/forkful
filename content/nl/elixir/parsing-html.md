---
title:                "HTML Parsen"
date:                  2024-01-28T22:03:41.766722-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML Parsen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/elixir/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

HTML parsen betekent het doorzoeken van HTML-code om gegevens of details programmatisch te extraheren. Programmeurs doen dit voor taken zoals webscraping, datamining of het automatiseren van interacties met websites.

## Hoe te:

In Elixir kun je HTML parsen met de Floki-bibliotheek. Hier is een fragment:

```elixir
# Voeg eerst Floki toe aan je mix.exs afhankelijkheden
{:floki, "~> 0.30.0"}

# Dan, in je code

defmodule HTMLParser do
  alias Floki

  def parse_html(html) do
    {:ok, document} = Floki.parse(html)
    titels = Floki.find(document, "h1")
    IO.inspect(titels, label: "Titels")
  end
end

# Gebruik
html_inhoud = "<html><body><h1>Hallo, Elixir!</h1></body></html>"
HTMLParser.parse_html(html_inhoud)

# Voorbeelduitvoer
Titels: [{"h1", [], ["Hallo, Elixir!"]}]
```

## Diepgaande Duik

Historisch gezien was HTML-parsing in talen zoals Python of JavaScript gebruikelijker, maar door de gelijktijdige functies en schaalbaarheid van Elixir staat het sterk als alternatief voor moderne webtaken. De Floki-bibliotheek gebruikt de fast_html C-parser voor snelheid, waardoor je het beste van twee werelden krijgt: de gelijktijdigheid van Elixir en de prestaties van een gecompileerde taal.

Vergeleken met andere tools zoals BeautifulSoup in Python, is Floki minder uitgebreid en meer functioneel van stijl - passend bij de ethos van Elixir. Plus, je hebt de volledige kracht van het Erlang-ecosysteem voor fouttolerantie en distributie, als je groot denkt.

## Zie Ook

- [Floki op Hex](https://hex.pm/packages/floki) - Officiële Floki-documentatie.
- [HTML5ever](https://github.com/servo/html5ever) - Rust HTML-parser die fast_html aandrijft.
