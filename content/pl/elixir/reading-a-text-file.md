---
title:    "Elixir: Odczytywanie pliku tekstowego"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego warto czytać pliki tekstowe?

Czytanie plików tekstowych jest powszechną czynnością w programowaniu. Pliki tekstowe są wykorzystywane do przechowywania danych, konfiguracji aplikacji, czy też do przechowywania wyników wyjściowych programów. Jest to ważna umiejętność dla każdego programisty, niezależnie od używanego języka programowania.

## Jak czytać pliki tekstowe w Elixir?

Aby czytać pliki tekstowe w Elixir, można użyć funkcji ```File.read/1```. Przykładowy kod wyglądałby tak:

```elixir
# Otwarcie pliku tekstowego w trybie tylko do odczytu
{:ok, file} = File.open("plik.txt")

# Wczytanie zawartości pliku do zmiennej
file_content = File.read(file)

# Wyświetlenie zawartości pliku na ekranie
IO.puts(file_content)

# Zamknięcie pliku
File.close(file)
```

Output tego kodu będzie zawierał wszystkie linie tekstu znajdujące się w pliku "plik.txt". Można także użyć funkcji ```File.read!/1```, która zwróci zawartość pliku w postaci binarnej.

## Głębsze zagadnienia

Przy czytaniu plików tekstowych ważne jest zabezpieczenie się przed błędami. Aby to osiągnąć, można wykorzystać try/catch lub funkcje obsługi wyjątków. Przykładowy kod wyglądałby tak:

```elixir
try do
  file = File.read("plik.txt")
rescue
  :error -> IO.puts "Wystąpił błąd podczas czytania pliku."
end
```

W przypadku, gdy plik nie istnieje lub wystąpi inny problem podczas odczytu, funkcja ```File.read/1``` zwróci :error, a obsługujący ją blok try/catch wyświetli odpowiedni komunikat.

## Zobacz też

Możesz przeczytać więcej o czytaniu plików tekstowych w Elixir na stronach:

- http://elixir-lang.org/getting-started/io-and-the-file-system.html
- https://hexdocs.pm/elixir/File.html