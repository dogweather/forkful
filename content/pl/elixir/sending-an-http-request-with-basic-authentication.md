---
title:                "Elixir: Wysyłanie zapytania http z podstawową autoryzacją"
simple_title:         "Wysyłanie zapytania http z podstawową autoryzacją"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Dlaczego warto używać uwierzytelniania podstawowego przy wysyłaniu zapytania HTTP

Uwierzytelnianie podstawowe jest jednym z najprostszych sposobów na zabezpieczenie danych przesyłanych pomiędzy klientem a serwerem. Dzięki niemu możliwe jest również kontrolowanie dostępu do określonych zasobów lub funkcji serwisu. W tym artykule dowiesz się, jak w praktyce używać uwierzytelniania podstawowego podczas wysyłania zapytań HTTP w języku Elixir.

## Jak to zrobić

Zanim zaczniemy kodować, warto na chwilę zatrzymać się i przypomnieć sobie, czym dokładnie jest uwierzytelnianie podstawowe. Polega ono na przesyłaniu w nagłówku zapytania informacji o nazwie użytkownika i haśle w formie zakodowanej w Base64. Po odebraniu zapytania, serwer dekoduje te informacje i weryfikuje, czy użytkownik ma dostęp do żądanego zasobu lub funkcjonalności.

Teraz przejdźmy do kodowania w Elixir. Przede wszystkim, musimy wykorzystać bibliotekę `HTTPoison` do wysłania zapytania HTTP. Następnie, w nagłówku `Authorization`, musimy przesyłać zakodowane dane uwierzytelniające. Przykładowy kod wyglądałby tak:

```
HTTPoison.get(
  "https://www.example.com/api/users",
  headers: [Authorization: "Basic " <> Base.encode64("username:password")]
)
```

Gdzie `username` i `password` to odpowiednio nazwa użytkownika i hasło, które chcemy przesłać. W ten sposób, jeśli serwer jest skonfigurowany, aby wymagać uwierzytelnienia podstawowego, dostaniemy dostęp do zasobów.

## Głębsze wody

W przypadku uwierzytelniania podstawowego, podręczniki często zalecają stosowanie protokołu HTTPS w celu zabezpieczenia danych. Dlatego też, gdy wysyłamy zapytanie HTTPS, możemy użyć dodatkowego parametru `timeout` w celu ustalenia czasu oczekiwania na odpowiedź:

```
HTTPoison.get(
  "https://www.example.com/api/users",
  headers: [Authorization: "Basic " <> Base.encode64("username:password")],
  timeout: 5000
)
```

Pamiętaj jednak, że uwierzytelnianie podstawowe nie jest najbezpieczniejszą metodą uwierzytelniania i może być łatwo złamane przez osoby nieupoważnione. Dlatego też, jeśli zabezpieczenie danych jest dla ciebie ważne, warto zdecydować się na bardziej skuteczne metody uwierzytelniania.

## Zobacz także

- [Dokumentacja HTTPoison](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Przewodnik po uwierzytelnianiu podstawowym w Elixir](https://www.digitalocean.com/community/tutorials/how-to-use-basic-authentication-with-httpoison-in-elixir)