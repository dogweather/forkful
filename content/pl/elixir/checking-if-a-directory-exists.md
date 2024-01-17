---
title:                "Sprawdzanie istnienia katalogu"
html_title:           "Elixir: Sprawdzanie istnienia katalogu"
simple_title:         "Sprawdzanie istnienia katalogu"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Sprawdzanie, czy istnieje katalog, jest działaniem, które polega na weryfikacji, czy dany katalog znajduje się w określonym miejscu w systemie plików. Programiści często wykonują to działanie w celu upewnienia się, że dany katalog jest dostępny i może być wykorzystywany w dalszej części kodu.

## Jak to zrobić:

```Elixir
IO.puts File.dir?("folder") # Wypisze "true" jeśli katalog "folder" istnieje
IO.puts File.dir?("inne_folder") # Wypisze "false" jeśli katalog "inne_folder" nie istnieje
```

## Głębsza analiza:

1. Historia - sprawdzanie, czy istnieje katalog, jest powszechnie stosowane w programowaniu od dłuższego czasu i jest dostępne w większości języków programowania.
2. Alternatywy - w Elixir istnieje również funkcja ```File.exists?```, która sprawdza, czy istnieje plik lub katalog o podanej ścieżce.
3. Szczegóły implementacji - sprawdzanie, czy istnieje katalog, wykorzystuje funkcję ```File.dir?``` dostępną w standardowej bibliotece Elixira. Funkcja ta zwraca wartość typu boolean (true lub false) w zależności od wyniku działania.

## Zobacz także:

- Dokumentacja Elixira na temat sprawdzania, czy istnieje katalog: https://hexdocs.pm/elixir/File.html#dir?/1
- Przykładowy projekt na GitHubie wykorzystujący funkcję ```File.dir?```: https://github.com/ziyi-yan/exdabbix