---
title:                "Elixir: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, dlaczego warto wysyłać zapytania HTTP w swoim kodzie? Gdy tworzymy aplikacje internetowe, często musimy komunikować się z innymi serwerami, aby uzyskać dostęp do danych lub wykonania pewnych operacji. Właśnie wtedy wysyłanie zapytań HTTP jest niezbędną umiejętnością. W tym artykule dowiesz się, jak w łatwy sposób wykonać to zadanie za pomocą języka Elixir.

## Jak to zrobić

W Elixirze, aby wysłać zapytanie HTTP, musimy użyć modułu `HTTPoison`, który jest dostępny w bibliotece standardowej Elixira. Najpierw musimy zadeklarować środowisko wykonywania zapytania, takie jak metoda HTTP, URL, nagłówki i treść. Następnie możemy użyć funkcji `HTTPoison.request!/5`, aby wysłać zapytanie i otrzymać odpowiedź.

```Elixir
defmodule HTTPRequest do
  def get_request(url) do
    response = HTTPoison.request!(:get, url)
    IO.inspect response.status_code
    IO.inspect response.body
  end
end

HTTPRequest.get_request("https://www.example.com")
```

Powyżej przedstawiono prosty przykład kodu, który obrazuje, jak wysłać zapytanie GET do adresu URL i otrzymać odpowiedź. W odpowiedzi otrzymujemy status zapytania oraz treść, która może być przetworzona w dowolny sposób. W ten sposób możemy komunikować się z innymi serwerami i pobierać ważne informacje dla naszej aplikacji.

## Deep Dive

Podczas wysyłania zapytania HTTP, możemy dostosować środowisko wykonywania, aby uzyskać więcej szczegółów w odpowiedzi. Na przykład, jeśli chcemy ustawić nagłówki, możemy przekazać listę w trzecim argumencie funkcji `HTTPoison.request!/5`. Podobnie, jeśli chcemy wysłać zapytanie POST lub PUT, musimy przekazać dodatkową treść w czwartym argumencie. Możemy również użyć funkcji `HTTPoison.get!/3` lub `HTTPoison.post!/4`, które wykorzystują domyślne wartości dla odpowiednio metody oraz opcjonalnych danych.

```Elixir
defmodule HTTPRequest do
  def post_request(url, body) do
    headers = [{"Content-Type", "application/json"}]
    response = HTTPoison.post!(url, body, headers)
    IO.inspect response.status_code
    IO.inspect response.body
  end
end

HTTPRequest.post_request("https://www.example.com", "{ \"foo\": \"bar\" }")
```

W powyższym przykładzie, używając funkcji `post_request/2`, wysyłamy zapytanie POST z ustalonymi nagłówkami i ciałem w formacie JSON. W ten sposób możemy dostosować nasze żądania według naszych potrzeb.

## Zobacz też

- [Oficjalna dokumentacja Elixir](https://elixir-lang.org/docs.html)
- [HTTPoison moduł](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Inne popularne banery Elixir](https://www.codementor.io/blog/top-elixir-blogs-9avqbqwh1d)