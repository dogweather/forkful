---
title:                "Tworzenie tymczasowego pliku"
html_title:           "C#: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Tworzenie tymczasowego pliku to proces generowania pliku, który jest potrzebny tylko przez krótki okres czasu. Programiści robią to, aby magazynować dane, których nie potrzebują na stałe, ale które są niezbędne dla jakiejś tymczasowej operacji.

## Jak to zrobić:

Używając Elixira, możemy łatwo utworzyć tymczasowy plik za pomocą modułu `File` . Poniżej znajduje się kod potrzebny do utworzenia tymczasowego pliku.

```elixir
File.write!("/tmp/temp.txt", "Oto jakiś tymczasowy tekst")
```

A oto, jak możemy zobaczyć zawartość pliku:

```elixir
IO.puts File.read!("/tmp/temp.txt")
```

Wynik powinien wyglądać mniej więcej tak:

```elixir
"Oto jakiś tymczasowy tekst"
```

## Głębsze rozeznanie

Tworzenie tymczasowych plików to praktyka, która sięga początków informatyki, kiedy dostęp do pamięci operacyjnej był ograniczony, a programiści musieli opracować sposoby na tymczasowe przechowywanie danych. Alternatywą jest użycie bazy danych do przechowywania tymczasowych danych, ale tworzenie tymczasowego pliku jest zazwyczaj prostszym i szybszym rozwiązaniem. Co do szczegółów implementacji, Elixir korzysta z funkcji systemu operacyjnego do utworzenia tymczasowych plików, a nazwa pliku jest generowana automatycznie, aby zapewnić unikalność.

## Zobacz także 

1. Dokumentacja Elixira na temat modułu `File`: https://hexdocs.pm/elixir/File.html
2. Artykuł na temat korzystania z tymczasowych plików w Elixer: https://www.elixir.school/basics/collections/ 
3. Przegląd technik zarządzania plikami w Elixer: https://elixirschool.com/en/lessons/specifics/file_io/