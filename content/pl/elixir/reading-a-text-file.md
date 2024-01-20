---
title:                "Czytanie pliku tekstowego"
html_title:           "C: Czytanie pliku tekstowego"
simple_title:         "Czytanie pliku tekstowego"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Czytanie plików tekstowych to proces odczytywania danych z plików, które są zapisane jako tekst. Programiści wykonują to do załadowania danych z plików do swojego kodu, co umożliwia pracę na tych danych lub ich użycie do generowania wyników.

## Jak to zrobić:

Elixir nadaje się świetnie do odczytywania plików tekstowych. Przyjrzyjmy się krótkiemu przykładowi:

```elixir
{:ok, zawartosc} = File.read("moj_plik.txt")
IO.puts zawartosc
```
W wyniku tego kodu, otrzymasz zawartość twojego pliku tekstowego 'moj_plik.txt' wydrukowaną w konsoli.

## W Głębi Tematu:

Historia: Odczytywanie plików tekstowych to jedna z najstarszych funkcji komputerowych, która była kiedyś kluczowym elementem programowania.

Alternatywy: W Elixirze, możemy również odczytywać pliki używając strumieni, co jest szczególnie użyteczne przy obsłudze dużych plików, które mogą nie pasować do pamięci. 

Szczegóły Implementacji: Elixir korzysta z :file Erlang’a do obsługi I/O operacji na plikach. Również, Elixir korzysta z podejścia obsługi I/O opartego na procesach, co umożliwia łatwe i wydajne zarządzanie plikami.

## Zobacz Również:

1. Dokumentacja Elixir: odczytywanie plików: https://hexdocs.pm/elixir/File.html#read/1
2. Kurs Elixira z poziomu podstawowego do zaawansowanego, gdzie jednym z tematów jest odczyt plików: https://elixirschool.com/en/
3. Blog o zaawansowanych technikach odczytywania plików w Elixirze: https://medium.com/@n2o/reading-large-files-in-elixir-revisited-8b959799e374