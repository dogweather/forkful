---
title:                "Ladda ner en webbsida"
html_title:           "Bash: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Nedladdning av en webbsida i Elixir 

## Vad & Varför?
Nedladdning av en webbsida innebär att hela webbsidans innehåll hämtas och sparats i din dator. Programmerare gör det för att extrahera och bearbeta platsens information, övervaka sidans ändringar eller bygga en webbscraping bot.

## Så här gör du
Låt oss använda HTTPoison biblioteket. För att installera det, lägg till det i din mix.exs-fil:

```elixir
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end
```

Kör sedan `mix deps.get` i din terminal för att hämta biblioteket. 

Vi kan nu hämta en webbsida:

```elixir
{:ok, response} = HTTPoison.get("https://example.com")
IO.puts(response.body)
```
Om du kör detta kommer det att skriva ut HTML-innehållet i "https://example.com".

## Fördjupning
Historiskt sett, användes kommando-rad verktyg som 'wget' och 'curl' för att ladda ner webbsidor. Elixir, en ungare språk, tar ett mer modern tillvägagångssätt genom att använda HTTP-klientbibliotek. 

Ett alternativ till HTTPoison skulle vara Tesla, som erbjuder middleware support. 

Elixir använder ett öppet samtidigt system som gör det möjligt att hämta flera webbsidor i bakgrunden, vilket ökar effektiviteten. 

## Se också
HTTPoison dokumentation: https://hexdocs.pm/httpoison/readme.html
Tesla på Hex: https://hex.pm/packages/tesla
Elixir-lang: https://elixir-lang.org