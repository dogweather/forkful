---
title:                "Tolka HTML"
date:                  2024-01-20T15:30:59.980594-07:00
html_title:           "Arduino: Tolka HTML"
simple_title:         "Tolka HTML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att parsa HTML innebär att vi läser och tolkar HTML-kod för att förstå dess struktur och innehåll. Programmörer gör detta för att extrahera specifik data, manipulera innehållet, eller för att integrera det med andra applikationer.

## Hur gör man:
Elixir erbjuder inte inbyggt stöd för HTML-parsing, så vi använder biblioteket `Floki` som underlättar processen. Installera Floki med `mix deps.get`.

```elixir
# Lägg till i mix.exs:
defp deps do
  [
    {:floki, "~> 0.30.0"}
  ]
end

# Exempel på att använda Floki för att hämta titeln från en HTML-sida

html = """
<!DOCTYPE html>
<html>
<head>
    <title>Exempelsida</title>
</head>
<body>
    <h1>Välkommen till exempelsidan!</h1>
    <p>Det här är en paragraph.</p>
</body>
</html>
"""

{:ok, document} = Floki.parse_document(html)
title = Floki.find(document, "title")
              |> Floki.raw_text()

IO.puts title # Skriver ut "Exempelsida"
```

## Djupdykning:
Parsing av HTML är inte en ny idé. I tidiga webbutvecklingsdagar fick man ofta använda regex för att hantera HTML, vilket var opålitligt och svårt. Alternativet till libraries som Floki för Elixir är att bygga en egen parser, vilket ofta inte är värt mödan då det är tidskrävande och kräver sträng uppmärksamhet på specifikationer och felhantering. Floki bygger på `mochiweb's HTML parser` och omvandlar HTML-strängar till tupler som är lätta att navigera och söka igenom.

## Se även:
- Floki on Hex: [https://hex.pm/packages/floki](https://hex.pm/packages/floki)
- Dokumentation för mochiweb HTML parser: [https://github.com/mochi/mochiweb](https://github.com/mochi/mochiweb)