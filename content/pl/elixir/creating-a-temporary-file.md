---
title:                "Elixir: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami, podczas pisania kodu w Elixir, musimy utworzyć plik tymczasowy, który służy jako bufor do przechowywania danych. Może to być przydatne na przykład podczas pobierania plików z sieci lub gdy chcemy przekazać dane do innej aplikacji. W tym artykule dowiesz się, jak i dlaczego tworzyć tymczasowe pliki w Elixir.

## Jak to zrobić

Utworzenie tymczasowego pliku w Elixir jest bardzo proste. Musimy użyć funkcji `File.open/2` i przekazać mu ścieżkę oraz tryb `:w` (zapis). Następnie możemy zapisywać do pliku za pomocą funkcji `IO.write/2` lub `IO.binwrite/2`. Przykładowy kod wygląda następująco:

```elixir
path = "./tempfile.txt" 
{:ok, file} = File.open(path, [:w])
IO.write(file, "To jest przykładowy tekst.")
IO.close(file)
```

Po uruchomieniu powyższego kodu, w aktualnym katalogu powinien pojawić się plik `tempfile.txt` zawierający tekst "To jest przykładowy tekst." W celu usunięcia pliku, możemy użyć funkcji `File.rm/1` podając jej ścieżkę do pliku.

## Głębszy wgląd

Istnieją również inne tryby otwierania plików w Elixir, takie jak `:r` (odczyt), `:a` (dopisywanie) czy `:rw` (odczyt i zapis). Możemy również wykorzystać funkcję `File.temp_path/1` do wygenerowania losowej ścieżki do tymczasowego pliku. Ponadto, warto pamiętać o upewnieniu się, czy plik został poprawnie otwarty poprzez sprawdzenie wyniku zwracanego przez funkcję `File.open/2`.

## Zobacz również

Oto kilka przydatnych linków, które mogą Ci się przydać:

- [Dokumentacja Elixir na temat obsługi plików](https://hexdocs.pm/elixir/File.html)
- [Wprowadzenie do Elixir](https://elixir-lang.org/getting-started/introduction.html)
- [Dokumentacja Markdown](https://daringfireball.net/projects/markdown/syntax)