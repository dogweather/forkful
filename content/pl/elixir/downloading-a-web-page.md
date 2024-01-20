---
title:                "Pobieranie strony internetowej"
html_title:           "C#: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Własne, dynamiczne pobieranie stron internetowych z programu jest jednym z podstawowych aspektów tworzenia aplikacji internetowych. Pozwala to na gromadzenie i analizę danych, automatyzację akcji i tworzenie własnych widoków na bazie zawartości innych stron.

## Jak to zrobić:

Aby pobrać stronę internetową w języku Elixir, użyjemy paczki HTTPoison. Oto proste przykładowe użycie:

```elixir
defmodule MyModule do
  def fetch_page do
    case HTTPoison.get("http://example.com") do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        IO.puts "Pobrano stronę pomyślnie"
        IO.puts body
      {:ok, %HTTPoison.Response{status_code: status_code}} ->
        IO.puts "Nie udało się pobrać strony, kod statusu #{status_code}"
      {:error, %HTTPoison.Error{reason: reason}} ->
        IO.puts "Error: #{reason}"
    end
  end
end

MyModule.fetch_page()
```

Ten kod synchronicznie pobiera zawartość strony internetowej "http://example.com" i wypisuje ją na ekran. W przypadku błędu, wydrukuje on informację o błędzie.

## Pogłębione informacje

Pobieranie stron internetowych ma długą historię w programowaniu, na początku obejmowało to zwykłe polecenia telnet, a potem protokołów HTTP z użyciem bibliotek jak curl w C. Elixir, jako nowoczesny język napędzający web, oferuje bardziej przyjazne rozwiązania jak HTTPoison czy Tesla.

Alternatywą dla HTTPoison może być Tesla, druga popularna paczka do wykonywania żądań HTTP w Elixir. Jej siłą jest wysoce konfigurowalne middleware, które pozwala na wygodne dostosowanie requestów i odpowiedzi.

Szczegółem implementacyjnym HTTPoison jest fakt, że korzysta on z hackney, niskopoziomowej biblioteki Erlangowej do obsługi HTTP.

## Zobacz także:

- Dokumentacja HTTPoison: https://hexdocs.pm/httpoison/readme.html
- Dokumentacja Tesla: https://hexdocs.pm/tesla/readme.html
- Dokumentacja hackney: https://hexdocs.pm/hackney/readme.html