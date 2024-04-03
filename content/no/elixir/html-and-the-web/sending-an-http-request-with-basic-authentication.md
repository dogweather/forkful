---
date: 2024-01-20 18:01:15.649768-07:00
description: "Hvordan: I Elixir kan `HTTPoison` benyttes for HTTP-foresp\xF8rsler\
  \ med grunnleggende autentisering. Her er et eksempel."
lastmod: '2024-03-13T22:44:40.443299-06:00'
model: gpt-4-1106-preview
summary: "I Elixir kan `HTTPoison` benyttes for HTTP-foresp\xF8rsler med grunnleggende\
  \ autentisering."
title: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende autentisering"
weight: 45
---

## Hvordan:
I Elixir kan `HTTPoison` benyttes for HTTP-forespørsler med grunnleggende autentisering. Her er et eksempel:

```elixir
# Først, legg til HTTPoison i deps i mix.exs:
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end

# Så, utfør en forespørsel med Basic Auth:
def send_basic_auth_request do
  auth = :base64.encode("bruker:passord")
  headers = [{"Authorization", "Basic #{auth}"}]

  HTTPoison.get("https://eksempel.com/beskyttet", headers)
end
```

Kjører du denne funksjonen får du et svar som dette:

```elixir
%HTTPoison.Response{
  status_code: 200,
  body: "...",
  headers: [...]
}
```

## Dypdykk
Grunnleggende autentisering har eksistert siden tidlig i HTTPs historie, spesifisert i RFC 7617. Enkelt, men mindre sikker enn moderne metoder, da det sender bruker-info i klar tekst (base64-kodet, ikke kryptert). 

Alternativer inkluderer OAuth og API-nøkler. Ved å bruke en egnet bibliotek som `HTTPoison` eller `Tesla`, blir implementering av disse autentiseringsmetodene mer håndterbar i Elixir.

For å forsterke sikkerheten, bør grunnleggende autentisering alltid brukes over HTTPS. Elixir's støtte for SSL gjør dette enkelt i både `HTTPoison` og `Tesla`. Potensielle feil i koden kan føre til lekkasje av sensitiv info; god håndtering av auth headeren er essensielt.

## Se Også
- Elixir's HTTPoison dokumentasjon: https://hexdocs.pm/httpoison
- RFC 7617, "The 'Basic' HTTP Authentication Scheme": https://tools.ietf.org/html/rfc7617
- Elixir's Tesla dokumentasjon: https://github.com/teamon/tesla
- En introduksjon til nettverkssikkerhet og HTTPS: https://www.ssl.com/faqs/faq-what-is-https/
