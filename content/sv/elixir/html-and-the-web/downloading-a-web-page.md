---
title:                "Hämta en webbsida"
aliases:
- sv/elixir/downloading-a-web-page.md
date:                  2024-01-20T17:43:59.723357-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hämta en webbsida"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Att ladda ner en webbsida innebär att hämta dess innehåll över internet så att det kan användas lokalt på din dator. Programmerare gör detta för att bearbeta data, testa webbsidor eller för att skapa en kopia av sidans innehåll.

## How to:
Elixir gör det lätt att ladda ner webbsidor med hjälp av biblioteket HTTPoison. Här är ett enkelt exempel som visar hur du gör:

```Elixir
# Först, lägg till HTTPoison i din mix.exs fil
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end

# Kör sedan mix deps.get för att installera beroenden.

# Efter det, använd följande kod för att ladda ner en webbsida:
HTTPoison.start()
case HTTPoison.get("https://example.com") do
  {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
    IO.puts("Innehållet på sidan: #{body}")
  {:ok, %HTTPoison.Response{status_code: status_code}} ->
    IO.puts("Kunde inte ladda sidan. Statuskod: #{status_code}")
  {:error, %HTTPoison.Error{reason: reason}} ->
    IO.puts("Ett fel uppstod: #{reason}")
end
```

Exempel på utskrift:
```
Innehållet på sidan: <!doctype html> ...
```

## Deep Dive
Förr i tiden användes ofta inbyggda funktioner som `:httpc` i Erlang för att ladda ner webbsidor i Elixir. Nuförtiden är HTTPoison det populärare valet eftersom det erbjuder en mer användarvänlig syntax och hanterar många av de komplexiteter som är förknippade med HTTP-kommunikation.

Alternativet till HTTPoison kan vara bibliotek som Tesla eller Req som också tillhandahåller enkelhet och flexibilitet i att göra HTTP-förfrågningar.

När du implementerar nedladdning av en webbsida, glöm inte att hantera olika HTTP-statuskoder och potentiella fel på ett elegant sätt. Det är också viktigt att respektera webbsidors robot.txt filer och använda rätt user-agent strängar så att din skrapning är ansvarsfull.

## See Also
- HTTPoison dokumentation: https://hexdocs.pm/httpoison/HTTPoison.html
- Elixir School för att lära dig mer om Elixir: https://elixirschool.com/en/
- "Programming Phoenix" bok för mer djupgående webbutveckling med Elixir: https://pragprog.com/titles/phoenix14/programming-phoenix-1-4/
