---
title:                "Wysyłanie żądania http"
html_title:           "Arduino: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Czym to jest i dlaczego?

Wysyłanie żądania HTTP to proces komunikacji między klientem (np. przeglądarką) a serwerem. Programiści wysyłają te żądania, aby pobrać informacje z innej strony, usługi API, lub każdego innego serwera.

## Jak to zrobić:

White dwa sposoby na wykonanie HTTP request przy użyciu Elixira: ```Httpoison``` i ```HTTPotion```.

Pierwszy to ```Httpoison```. Oto jak to zrobić:

```Elixir
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end
```

Teraz, uruchom:

```Elixir
mix deps.get
```

I oto kod:

```Elixir
HTTPoison.start
response = HTTPoison.get!("https://jsonplaceholder.typicode.com/posts")
IO.puts response.body
```
Oto jak używać ```HTTPotion```. Najpierw dodaj go do pliku mix.exs:

```Elixir
defp deps do
  [
    {:httpotion, "~> 3.1"}
  ]
end
```

Teraz, uruchom:

```Elixir
mix deps.get
```

I taki będzie kod:

```Elixir
HTTPotion.start
response = HTTPotion.get "https://jsonplaceholder.typicode.com/posts"
IO.puts response.body
```

## Dogłębnie

Zapytania HTTP są fundamentalnym blokiem budowania większości aplikacji internetowych. HTTP semantyka zapytań jest starszą technologią, która sięga początków internetu i Web 2.0. Istnieje wiele sposobów na wysyłanie żądań HTTP w Elixir, takich jak użycie modułu ```httpc``` w BEAM, ale ```Httpoison``` i ```HTTPotion``` są najpopularniejszymi ze względu na ich prostotę.

## Zobacz też

- Dokumentacja Httpoison: https://hexdocs.pm/httpoison/readme.html
- Dokumentacja HTTPotion: https://hexdocs.pm/httpotion/readme.html
- Moduł httpc w BEAM: http://erlang.org/doc/man/httpc.html