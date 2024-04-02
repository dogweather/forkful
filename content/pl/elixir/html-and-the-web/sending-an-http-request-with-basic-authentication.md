---
date: 2024-01-20 18:01:36.128761-07:00
description: "Wysy\u0142anie \u017C\u0105dania HTTP z podstawow\u0105 autentykacj\u0105\
  \ to metoda zabezpieczenia, dzi\u0119ki kt\xF3rej serwer mo\u017Ce zweryfikowa\u0107\
  \ to\u017Csamo\u015B\u0107 u\u017Cytkownika za pomoc\u0105 loginu i\u2026"
lastmod: '2024-03-13T22:44:35.041872-06:00'
model: gpt-4-1106-preview
summary: "Wysy\u0142anie \u017C\u0105dania HTTP z podstawow\u0105 autentykacj\u0105\
  \ to metoda zabezpieczenia, dzi\u0119ki kt\xF3rej serwer mo\u017Ce zweryfikowa\u0107\
  \ to\u017Csamo\u015B\u0107 u\u017Cytkownika za pomoc\u0105 loginu i\u2026"
title: "Wysy\u0142anie zapytania http z podstawow\u0105 autoryzacj\u0105"
weight: 45
---

## Co i dlaczego?

Wysyłanie żądania HTTP z podstawową autentykacją to metoda zabezpieczenia, dzięki której serwer może zweryfikować tożsamość użytkownika za pomocą loginu i hasła. Programiści używają tego mechanizmu, aby ograniczyć dostęp do zasobów na serwerze tylko dla upoważnionych użytkowników.

## Jak to zrobić:

Aby wysłać żądanie HTTP z podstawową autentykacją w Elixirze, możesz użyć biblioteki HTTP klienta, np. `HTTPoison`. Poniżej znajdziesz przykład kodu wraz z przykładową odpowiedzią serwera.

```elixir
# Musisz dodać HTTPoison do twojej listy zależności w mix.exs
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end

def send_request_with_basic_auth do
  auth_header = encode_credentials("username", "password")

  HTTPoison.get(
    "http://example.com/protected/resource", 
    [{"Authorization", auth_header}]
  )
end

defp encode_credentials(username, password) do
  "Basic " <> Base.encode64("#{username}:#{password}")
end
```

Po uruchomieniu `send_request_with_basic_auth/0` otrzymasz tuple z odpowiedzią lub błędem:

```elixir
{:ok, %HTTPoison.Response{status_code: 200, body: body}} # Sukces
{:error, %HTTPoison.Error{reason: reason}} # Błąd
```

## Deep Dive

Podstawowa autentykacja (basic auth) jest prostym mechanizmem kontrolującym dostęp, stosowanym w protokole HTTP już od wczesnych lat jego istnienia. Alternatywnymi metodami są OAuth, API keys czy JWT (JSON Web Tokens), które oferują różne poziomy bezpieczeństwa i wygody.

Implementacja podstawowej autentykacji w Elixirze jest bezpośrednia: zakoduj login i hasło w formacie Base64, dołącz jako nagłówek `Authorization` w twoim żądaniu HTTP. Warto zwrócić uwagę, że metoda ta przesyła dane uwierzytelniające w przesyłalnych tekstach, co nie jest rekomendowane na produkcji bez SSL/TLS.

Ważne jest również zrozumienie, że wiele bibliotek HTTP w Elixirze, takich jak HTTPoison czy Tesla, oferują wewnętrzne wsparcie dla autentykacji, więc nie musisz ręcznie kodować nagłówków.

## Zobacz również

- Dokumentacja HTTPoison: https://hexdocs.pm/httpoison/HTTPoison.html
- OAuth 2.0 guide: https://oauth.net/2/
- Przykład użycia JWT w Elixirze: https://github.com/joken-elixir/joken
- Elixir School – lekcje o HTTP klientach: https://elixirschool.com/pl/lessons/libraries/http_clients/
