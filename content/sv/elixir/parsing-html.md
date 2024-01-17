---
title:                "Att tolka html"
html_title:           "Elixir: Att tolka html"
simple_title:         "Att tolka html"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?
HTML är det språk som används för att strukturera webbsidor och innehåll på internet. Genom parsing, eller bearbetning, av HTML-kod kan vi extrahera och manipulera data från en webbsida i vårt Elixir-program. Detta är särskilt användbart när vi vill automatisera processer eller skapa webbskrapare.

## Så här:
```Elixir
# Installera Paketet Floki i ditt projekt
mix deps.get floki

# Importera Floki modulen i ditt Elixir-program
import Floki

# Hämta en webbsidas HTML-kod
html = Floki.parse_file("index.html")

# Extrahera en specifik tagg eller klass från HTML-koden
tagg = Floki.find(html, "span")
klass = Floki.find(html, ".rubrik")
```

## Djupdykning:
Parsning av HTML är en viktig del av webbutveckling och automatisering av webbprocesser. Andra alternativ för HTML-parsing i Elixir inkluderar paketet Drab och plugindelen HTML Kontrol. Genom att använda regelbundna uttryck i kombination med Floki kan vi också skriva mer avancerade extractionsskript.

## Se även:
- [Floki på Hexdocs](https://hexdocs.pm/floki/readme.html)
- [Drab på Hexdocs](https://hexdocs.pm/drab/readme.html)
- [HTML Kontrol på Hexdocs](https://hexdocs.pm/html_kontrol/readme.html)