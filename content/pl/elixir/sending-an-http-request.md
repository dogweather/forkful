---
title:                "Przesyłanie żądania http"
html_title:           "Elixir: Przesyłanie żądania http"
simple_title:         "Przesyłanie żądania http"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co to jest i po co to robimy?
Wysyłanie zapytania HTTP to sposób na komunikację z internetowymi serwisami i aplikacjami. Programiści często wykonują to działanie w celu pobrania lub przetworzenia danych dostępnych online.

## Jak to zrobić:
Kilka przykładów kodu z wyjściem dla użycia Elixira w wysyłaniu zapytań HTTP:

```Elixir
# Użycie wbudowanej biblioteki HTTPoison:
HTTPoison.get("https://www.example.com")
# => {:ok,
#    %HTTPoison.Response{
#      status_code: 200,
#      headers: [...],
#      body: "Hello world!"
#    }
#   }

# Użycie zewnętrznej biblioteki tesla:
Tesla.get("https://www.example.com")
# => {:ok,
#     %HTTPoison.Response{
#       status_code: 200,
#       headers: [...],
#       body: "Hello world!"
#     }
#    }

# Użycie wbudowanej biblioteki Erlanga:
:inet.request(:get, {"https://www.example.com", []}, [], [])
# => {:ok, %HTTPPoison.Response {...}}
```

## Ciekawostki:
Erlang, na którym oparta jest Elixir, jest często używany do tworzenia szybkich i niezawodnych serwerów sieciowych. Dzięki temu, wysyłanie zapytań HTTP w Elixir jest szybkie i skuteczne.

## Zobacz też:
- [Dokumentacja Elixir o wysyłaniu zapytań HTTP](https://hexdocs.pm/elixir/HTTPoison.html)
- [Porównanie bibliotek HTTP w Elixir](https://blog.distortedthinking.agency/elixir-http-libraries-compared/)