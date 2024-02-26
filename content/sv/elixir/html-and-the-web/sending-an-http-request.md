---
date: 2024-01-20 17:59:30.011559-07:00
description: "Att skicka en HTTP-beg\xE4ran inneb\xE4r att be en webbserver om data\
  \ eller en annan resurs. Programmerare g\xF6r detta f\xF6r att integrera olika webbtj\xE4\
  nster, h\xE4mta\u2026"
lastmod: '2024-02-25T18:49:35.904892-07:00'
model: gpt-4-1106-preview
summary: "Att skicka en HTTP-beg\xE4ran inneb\xE4r att be en webbserver om data eller\
  \ en annan resurs. Programmerare g\xF6r detta f\xF6r att integrera olika webbtj\xE4\
  nster, h\xE4mta\u2026"
title: "Skicka en http-f\xF6rfr\xE5gan"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-begäran innebär att be en webbserver om data eller en annan resurs. Programmerare gör detta för att integrera olika webbtjänster, hämta information eller interagera med API:er.

## Hur man gör:
I Elixir använder vi ofta `HTTPoison` för HTTP-begäran. Så här ser en grundläggande GET-begäran ut:

```elixir
# Lägg först till HTTPoison i dina dependencies i mix.exs
{:httpoison, "~> 1.8"}

# Starta ett iex session med `iex -S mix` och kör:
HTTPoison.start()

# Exempel på att skicka en GET-begäran
{:ok, response} = HTTPoison.get("https://jsonplaceholder.typicode.com/posts/1")

# Inspektera svaret
IO.inspect(response)
```

Svarsexempel:

```elixir
%HTTPoison.Response{
  body: "{...}",
  status_code: 200,
  ...
}
```

## Fördjupning
HTTP-begäran är sprungen ur HTTP-protokollet, standarden för kommunikation på webben sedan början av 90-talet. Alternativ till `HTTPoison` inkluderar `Tesla` och Elixirs inbyggda `HTTP`-klient ` :httpc`. Hur vi hanterar HTTP-begäran i Elixir bygger på Erlangs robusta `:inet` och `:ssl` applikationer, vilket innebär att prestanda och säkerhet är inbäddade från start.

## Se också
- Elixir's officiella dokumentation för HTTP-klienten `:httpc`: https://hexdocs.pm/elixir/1.13/HTTP.html
- HTTPoison GitHub-repo: https://github.com/edgurgel/httpoison
- Tesla GitHub-repo: https://github.com/teamon/tesla
