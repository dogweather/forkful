---
date: 2024-01-20 18:01:47.128414-07:00
description: "Steg f\xF6r steg Exempel output."
lastmod: '2024-04-05T22:37:46.252332-06:00'
model: gpt-4-1106-preview
summary: "Steg f\xF6r steg Exempel output."
title: "Skicka en HTTP-f\xF6rfr\xE5gan med Basic-autentisering"
weight: 45
---

## Steg för steg
```elixir
# Använd HTTPoison-biblioteket – lägg till det i din mix.exs fil:
defp deps do
  [{:httpoison, "~> 1.8"}]
end

# Exempel på att skicka en HTTP GET-request med basautentisering:
def send_authenticated_request(url, username, password) do
  # Skapa en autentiseringssträng
  auth_string = Base.encode64("#{username}:#{password}")

  # Ställ in headers för autentisering
  headers = ["Authorization": "Basic #{auth_string}"]

  # Skicka begäran
  HTTPoison.get(url, headers)
end

# Anropa funktionen och hantera svaret
case send_authenticated_request("https://example.com/protected", "user", "pass") do
  {:ok, response} -> IO.inspect(response.body)
  {:error, error} -> IO.inspect(error)
end
```
Exempel output:
```
"<p>Du har åtkomst till den skyddade resursen.</p>"
```

## Djupdykning
Basautentisering är ett av de äldsta sätten att skydda webbresurser, introducerat med HTTP 1.0. Trots att det är enkelt är det inte det säkraste — lösenorden är endast bas64-kodade, inte krypterade. Det är viktigt att använda HTTPS för att skydda uppgifterna i överföringen.

Alternativ inkluderar OAuth och JWT (JSON Web Tokens) som erbjuder bättre säkerhet och funktionalitet för moderna applikationer.

HTTPoison är en populär Elixir-bibliotek för att göra HTTP-anrop men det finns andra som Tesla eller traditionella :httpc i Erlang.

När vi implementerar basautentisering måste vi komma ihåg att hantera fel och försöka igen, särskilt vid hantering av nätverk eller service avbrott.

## Se även
- Elixir HTTPoison documentation: https://hexdocs.pm/httpoison
- Erlang :httpc module documentation: http://erlang.org/doc/man/httpc.html
- Understanding HTTP Authentication: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Alternatives to Basic Auth: Look into OAuth (https://oauth.net/2/) and JWT (https://jwt.io/introduction/)
