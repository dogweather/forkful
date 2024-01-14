---
title:                "Elixir: Pisanie pliku tekstowego"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

### Dlaczego pisanie plików tekstowych jest ważne w programowaniu?

Pisanie plików tekstowych jest ważną częścią procesu programowania, ponieważ pozwala nam przechowywać i przetwarzać duże ilości informacji w prosty i czytelny sposób. Pliki tekstowe są również przydatne, gdy chcemy udostępnić nasz kod innym programistom lub uruchomić naszą aplikację na różnych środowiskach.

### Jak pisać pliki tekstowe w języku Elixir?

W języku Elixir możemy łatwo pisać pliki tekstowe przy użyciu wbudowanych funkcji modułu `File` oraz `IO`. Poniżej znajdują się przykładowe kodowe bloki przedstawiające dwa sposoby zapisu tekstu do pliku:

```elixir
# Przy użyciu modułu File
File.write("moj_plik.txt", "To jest przykładowy tekst.")

# Przy użyciu modułu IO
File.open("moj_plik.txt", [:write], fn file ->
  IO.write(file, "To jest przykładowy tekst.")
end)
```

W powyższych przykładach `moj_plik.txt` jest nazwą pliku, a `"To jest przykładowy tekst."` jest tekstem, który zostanie zapisany w pliku. Możemy również użyć różnych opcji, takich jak `:append` lub `:utf8`, aby dostosować sposób zapisu pliku.

Po wykonaniu powyższego kodu, utworzymy plik o nazwie `moj_plik.txt` z tekstem `"To jest przykładowy tekst."` wewnątrz.

### Pogłębione informacje o pisaniu plików tekstowych

Pisanie plików tekstowych w języku Elixir jest bardzo proste i wydajne, ponieważ język ten ma wbudowane funkcje, które pozwalają nam na łatwe zarządzanie plikami. Dodatkowo, dzięki funkcjom modułu `IO`, możemy nie tylko pisać do pliku, ale także czytać z niego i manipulować jego zawartością.

Warto również pamiętać, że pliki tekstowe w Elixir są traktowane jako strumienie danych, co sprawia, że są wysoce wydajne i mogą być przetwarzane w sposób przewidywalny i skalowalny.

### Zobacz również

- [Dokumentacja Elixir - Moduł File](https://hexdocs.pm/elixir/File.html)
- [Dokumentacja Elixir - Moduł IO](https://hexdocs.pm/elixir/IO.html)
- [Blog o Elixirze (po polsku)](https://brainhub.eu/blog/elixir-put-language-backend-development/)