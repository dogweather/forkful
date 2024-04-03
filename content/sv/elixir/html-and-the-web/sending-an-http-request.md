---
date: 2024-01-20 17:59:30.011559-07:00
description: "Hur man g\xF6r: I Elixir anv\xE4nder vi ofta `HTTPoison` f\xF6r HTTP-beg\xE4\
  ran. S\xE5 h\xE4r ser en grundl\xE4ggande GET-beg\xE4ran ut."
lastmod: '2024-03-13T22:44:37.563782-06:00'
model: gpt-4-1106-preview
summary: "I Elixir anv\xE4nder vi ofta `HTTPoison` f\xF6r HTTP-beg\xE4ran."
title: "Skicka en http-f\xF6rfr\xE5gan"
weight: 44
---

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
