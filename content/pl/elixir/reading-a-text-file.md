---
title:                "Odczytywanie pliku tekstowego"
html_title:           "Elixir: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Czym jest czytanie pliku tekstowego i dlaczego to robimy?
Czytanie pliku tekstowego to proces, w którym dane zapisane w pliku są odczytywane i przetwarzane przez program. Programiści często wykorzystują tę funkcję, aby uzyskać dostęp do danych lub konfiguracji, które są przechowywane w plikach tekstowych.

## Jak to zrobić:
```Elixir
File.read("nazwa_pliku.txt")
|> IO.puts 
```
W tym przykładzie wykorzystujemy funkcję `File.read` do odczytania zawartości pliku tekstowego o nazwie `nazwa_pliku.txt`. Następnie za pomocą funkcji `IO.puts` wyświetlamy odczytane dane w konsoli.

### Przykładowe wyjście:
```
To jest zawartość pliku tekstowego.
Wszystkie linijki zostaną wyświetlone w konsoli.
```

## Głębszy przegląd:
Odczytywanie plików tekstowych jest powszechną praktyką w programowaniu. Wcześniej funkcja ta była wykorzystywana głównie w językach programowania takich jak C czy Java, ale dzięki Elixirowi mamy prostsze i bardziej wygodne rozwiązanie.

Alternatywnym sposobem na odczytanie pliku tekstowego jest użycie funkcji `IO.read_file`, która zwraca dane w postaci binarnej lub zmienia typ danych na string za pomocą funkcji `IO.inspect`.

## Zobacz także:
Oficjalna dokumentacja Elixir: https://elixir-lang.org/docs.html
Przykłady wykorzystania funkcji File.read: https://hexdocs.pm/elixir/File.html#read/1