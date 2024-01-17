---
title:                "Tworzenie pliku tymczasowego"
html_title:           "Elixir: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Tworzenie tymczasowych plików w Elixirze to szybki i wygodny sposób na przechowywanie danych tylko na potrzeby bieżącego wykonania programu. Programiści używają go często do zapisywania tymczasowych danych lub do testowania kodu.

## Jak to zrobić:
Aby utworzyć tymczasowy plik w Elixirze, musisz wykorzystać moduł `Tempfile`. W poniższym przykładzie stworzymy plik o nazwie `temp.txt` w bieżącym katalogu, a następnie zapiszemy w nim dowolny tekst. 

```Elixir
file = Tempfile.open("temp.txt")
IO.write(file.path, "To jest przykładowy tekst.")
```

Aby potwierdzić, że plik został utworzony i zawiera nasz tekst, można wyświetlić jego zawartość za pomocą następującego polecenia:

```Elixir
IO.gets(file.path)
```

Wynikiem powinien być nasz zapisany tekst.

## Dogłębna analiza:
Tworzenie tymczasowych plików jest powszechnie stosowane przez programistów już od dawna. W starszych językach programowania, takich jak C czy Java, wymagało to bardziej skomplikowanych operacji i bardzo łatwo było popełnić błąd. Dzięki modułowi `Tempfile` w Elixirze, proces ten jest prostszy i nie wymaga od nas pamiętania o zamknięciu pliku po użyciu.

Alternatywą dla tworzenia tymczasowych plików w Elixirze jest używanie struktur danych, takich jak mapy czy listy. Jednak w niektórych przypadkach, na przykład w testowaniu kodu, potrzebujemy pliku o rzeczywistym formacie, a nie tylko danych w pamięci.

## Zobacz również:
- [Dokumentacja modułu Tempfile](https://hexdocs.pm/elixir/1.10.1/Tempfile.html)
- [Poradnik do programowania w Elixirze](https://elixir-lang.org/getting-started/introduction.html)
- [Tutorial: Tworzenie tymczasowych plików w Elixirze](https://hexdocs.pm/elixir/tempfile.html)