---
title:                "Pobieranie strony internetowej"
html_title:           "Elixir: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli chcesz pobrać stronę internetową, istnieje kilka możliwych powodów. Może chcesz przeglądać ją offline, odświeżić ją w celu uzyskania najnowszych informacji, albo po prostu nie masz dostępu do Internetu w danym momencie. Bez względu na powód, Elixir jest idealnym wyborem do tego zadania.

## Jak to zrobić

Aby pobrać stronę internetową w Elixir, musimy użyć modułu `HTTPoison` i funkcji `get`:

```Elixir
response = HTTPoison.get("https://example.com")
```

W tym przykładzie używamy `example.com` jako przykładu, ale możesz podać dowolny adres URL. Gdy wywołasz tę funkcję, otrzymasz odpowiedź HTTP do wykorzystania. Możesz wyświetlić jej kod odpowiedzi i treść, używając:

```Elixir
IO.puts(response.status_code)
IO.puts(response.body)
```

Gdy wklejasz ten kod do swojej aplikacji Elixir i uruchamiasz ją, powinieneś zobaczyć status 200 oraz treść strony internetowej `example.com` w konsoli.

## Deep Dive

Jeśli chcesz pobrać stronę internetową w formie tekstu, możesz wykorzystać funkcję `get_text` z modułu `HTTPoison`:

```Elixir
response = HTTPoison.get_text("https://example.com")
```

Ta funkcja zwraca tylko zawartość strony bez kodu HTML. Możesz również ustawić nagłówki, dane wysyłane, czas oczekiwania na odpowiedź i wiele innych opcji przy pomocy parametrów funkcji `get` i `get_text` oraz funkcji pomocniczych związanych z `HTTPoison`.

## Zobacz też

- Dokumentacja dla modułu `HTTPoison` - https://hexdocs.pm/httpoison/HTTPoison.html
- Instrukcja użycia dla Elixir - https://hexdocs.pm/elixir/getting-started.html