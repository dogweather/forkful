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

## Dlaczego

 Jeśli jesteś programistą Elixira, na pewno wiesz, że czasami potrzebujesz stworzyć tymczasowy plik w swoim kodzie. Może to mieć różne zastosowania, na przykład do tymczasowego przechowywania danych czy też do tworzenia kopii zapasowych. W tym artykule dowiecie się jak stworzyć tymczasowy plik w Elixirze i jakie są możliwe zastosowania.

## Jak to zrobić

Aby stworzyć tymczasowy plik w Elixirze, musisz wykorzystać jedną z bibliotek dostępnych w tym języku. Jedną z najpopularniejszych jest biblioteka ExFile, która oferuje wiele funkcji związanych z plikami.

Pierwszym krokiem jest zainstalowanie biblioteki w swoim projekcie. Można to zrobić poprzez dodanie jej do pliku "mix.exs" w sekcji "deps":
```
def deps do
  [{:ex_file, "~> 0.1.2"}]
end
```
Następnie należy wywołać funkcję "Temporary.file" z parametrem określającym nazwę pliku oraz ścieżkę, w której ma zostać utworzony. Przykładowo:
```
{:ok, file} = ExFile.Temporary.file("example", "/tmp")
```
Kod ten utworzy tymczasowy plik "example" w folderze "/tmp". W przypadku sukcesu, funkcja zwróci krotkę zawierającą atom ":ok" oraz stworzony plik. Można również podać dodatkowe opcje, takie jak rozszerzenie pliku czy też prefiks nazwy.

Możliwości w bibliotece ExFile są bardzo szerokie i warto zapoznać się z dokumentacją, aby poznać wszystkie dostępne funkcje. 

## Deep Dive

Stworzenie tymczasowego pliku może być bardzo przydatne w różnych sytuacjach. Niektóre z możliwych zastosowań to:

- tymczasowe przechowywanie danych w trakcie wykonywania programu
- zapisywanie stanu w razie awarii systemu
- tworzenie kopii zapasowych danych przed ich modyfikacją

Warto również pamiętać, że wiele kolejnych operacji na plikach, takich jak odczyt czy zapis, można wykonać wykorzystując właśnie tymczasowy plik.

## Zobacz też

Jeśli chcesz dowiedzieć się więcej o tworzeniu plików w Elixirze, polecamy zapoznanie się z poniższymi źródłami:

- [Dokumentacja biblioteki ExFile](https://hexdocs.pm/ex_file/ExFile.Temporary.html)
- [Wprowadzenie do tworzenia plików w Elixirze](https://medium.com/@ajaypai/draft-creating-files-in-a-functional-way-with-elixir-9f53be3ec8de)
- [Przykładowy projekt wykorzystujący tworzenie plików w Elixirze](https://github.com/ryo33/Elixir-Fake-CSV)