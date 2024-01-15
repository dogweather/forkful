---
title:                "Analysera html"
html_title:           "Elixir: Analysera html"
simple_title:         "Analysera html"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## Varför

Att extrahera data från HTML-dokument är en viktig uppgift inom webbutveckling. Oavsett om du behöver skrapa data från en webbsida, utföra SEO-analyser eller bygga en webskrapa, så är HTML-parsing en viktig del av processen. Genom att lära dig hur man gör det på rätt sätt med hjälp av Elixir, kan du effektivt och pålitligt hantera ditt HTML-innehåll.

## Hur man gör det

För att parsra HTML med Elixir, kan du använda biblioteket Floki. Här är ett exempel på kod som hämtar länkar från en webbsida och returnerar en lista med URL:er:

```
page = Floki.parse_document(html) 
links = Floki.find(page, "a") 
urls = Enum.map(links, fn link -> Floki.attribute(link, "href") end) 
```

Denna kod använder sig av Floki-funktioner för att hitta och hämta länkarna från HTML-sidan. Resultatet blir en lista med URL:er som du kan använda för att utföra olika åtgärder, som till exempel att besöka webbplatserna eller spara dem i en databas.

## Djupdykning

När du behöver hantera mer komplexa HTML-strukturer, kan du använda dig av Flokis CSS-selectors för att välja specifika delar av dokumentet. Till exempel, om du vill hämta alla bilder från en sida, kan du använda följande kod:

```
images = Floki.find(page, "img")
```

Du kan också använda Flokis inbyggda funktioner för att filtrera och manipulera HTML-innehållet. Till exempel, om du bara vill hämta länkar som innehåller en viss term, kan du använda följande kod:

```
links = Floki.find(page, "a", fn(el) -> String.contains?(Floki.attribute(el, "href"), "elixir") end)
```

Genom att utforska Flokis olika funktioner och möjligheter kan du effektivt hantera även de mest komplexa HTML-strukturerna.

## Se också

- Officiell Elixir-dokumentation om Floki: https://hexdocs.pm/floki/readme.html
- En introduktion till Elixir: https://elixir-lang.org/getting-started/introduction.html
- Elixir-skolan (på svenska): https://elixirskolan.se/