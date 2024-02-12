---
title:                "Skicka en HTTP-förfrågan med Basic-autentisering"
date:                  2024-01-20T18:01:47.128414-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skicka en HTTP-förfrågan med Basic-autentisering"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad och varför?
Att skicka en HTTP-begäran med basautentisering innebär att du använder användarnamn och lösenord för att få tillgång till en resurs på webben. Programmerare gör detta för att säkerställa åtkomst till skyddade resurser där enkelheten och standardiseringen av metoden är viktig.

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