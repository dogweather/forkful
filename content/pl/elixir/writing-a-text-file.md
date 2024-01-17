---
title:                "Tworzenie pliku tekstowego"
html_title:           "Elixir: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

Co i dlaczego?

Zapisywanie pliku tekstowego to proces przechowywania danych w pliku w formacie tekstowym z wykorzystaniem odpowiedniego kodowania. Programiści wykorzystują tę technikę, aby przechowywać i przetwarzać różnego rodzaju informacje, takie jak tekst, liczby czy inne dane.

Jak to zrobić:

```Elixir
# Otwarcie pliku tekstowego w trybie zapisu
File.open("plik.txt", [:write], fn file ->
  # Zapisanie tekstu do pliku
  IO.write(file, "To jest przykładowy tekst")
  # Zapisanie liczb do pliku
  IO.write(file, 123)
end)
```

Wynik w pliku "plik.txt" będzie wyglądać następująco:

```
To jest przykładowy tekst
123
```

W Elixir istnieje również wbudowana funkcja `File.write/2`, która pozwala na zapisanie danych bez użycia `IO.write`:

```Elixir
# Zapisanie tekstu do pliku
File.write("plik.txt", "To jest przykładowy tekst")
# Zapisanie liczb do pliku
File.write("plik.txt", 123)
```

Głębsze zagadnienia:

Zapisywanie plików tekstowych jest podstawowym procesem w wielu programach i językach programowania. Jest to również bardzo ważna umiejętność dla programisty do przetwarzania i przechowywania danych. Alternatywnym sposobem zapisywania danych jest wykorzystanie baz danych lub przechowywanie danych w pamięci podręcznej. W implementacji Elixir, jest to możliwe dzięki modułowi `File`.

Zobacz również:

- Dokumentacja Elixir dotycząca zapisywania plików: https://hexdocs.pm/elixir/File.html#write/2
- Tutorial dotyczący operacji na plikach w Elixir: https://elixir-lang.org/getting-started/file-operations.html
- Blog o podstawach pracy z plikami tekstowymi w Elixir: https://www.pluralsight.com/guides/elixir-text-files