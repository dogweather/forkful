---
title:                "Wysyłanie żądania http"
html_title:           "Elixir: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego ktoś chciałby wysłać żądanie HTTP? W dzisiejszych czasach wiele aplikacji wykorzystuje żądania HTTP do komunikacji z serwerami. Oznacza to, że nauka, jak wysyłać żądania HTTP w Elixirze, jest niezbędnym narzędziem dla każdego programisty.

## Jak To Zrobić

Wysyłanie żądania HTTP w Elixirze jest prostsze niż się wydaje. Wystarczy użyć funkcji `HTTPoison.get` i przekazać jej adres URL, z którego chcemy pobrać dane. Spójrzmy na przykładowy kod:

```Elixir
response = HTTPoison.get("https://www.example.com")

IO.puts(response.body)
```

Każda odpowiedź zwracana przez `HTTPoison` jest obiektem `HTTPoison.Response`, który ma wiele przydatnych metod, takich jak `status_code` czy `headers`. Przykładowy wynik wykonania powyższego kodu może wyglądać tak:

```Elixir
<!doctype html>
<html>
  <head>
    <title>Przykładowa strona</title>
  </head>
  <body>
    <h1>Witaj w przykładowej stronie!</h1>
  </body>
</html>
```

Zauważ, że pobrana odpowiedź jest w formacie HTML. W przypadku, gdy chcemy pobrać dane w formacie JSON, możemy skorzystać z funkcji `HTTPoison.get!/2` i dodać opcję `:headers` zgodną z `content-type: "application/json"`.

## Deep Dive

Aby zrozumieć, jak dokładnie działa wysyłanie żądań HTTP w Elixirze, warto dowiedzieć się o bibliotece `HTTPoison`. Jest ona oparta na popularnej bibliotece HTTP Client dla języka Ruby - `HTTParty`. Dzięki temu, wiele funkcji jest podobnych do tych, które wykorzystujemy w Ruby. Ponadto, dzięki użyciu wzorca "pipelining", kod wygląda bardzo wyraźnie i czytelnie.

Należy również zauważyć, że `HTTPoison` domyślnie używa procesów, co pozwala na równoległe wysyłanie wielu żądań jednocześnie. Dzięki temu, aplikacje napisane w Elixirze mogą łatwo obsługiwać wiele żądań jednocześnie.

## Zobacz również

- [Dokumentacja HTTPoison](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Strona główna Elixir](https://elixir-lang.org/)
- [Ruby HTTParty](https://github.com/jnunemaker/httparty)