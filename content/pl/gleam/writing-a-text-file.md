---
title:                "Gleam: Tworzenie pliku tekstowego"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Dlaczego warto pisać pliki tekstowe w programowaniu Gleam

Dzięki wykorzystaniu tekstowych plików, możesz przechowywać i przesyłać dane w łatwy sposób, co jest szczególnie przydatne w przypadku aplikacji internetowych. Pliki tekstowe są również łatwiejsze do odczytania i edycji przez człowieka niż inne formaty danych.

## Jak to zrobić

Poniżej znajdziesz kod w języku Gleam, który pokaże Ci, jak stworzyć i zapisać plik tekstowy:

```Gleam
// Importowanie modułu "std/fs", odpowiedzialnego za obsługę systemu plików
import std/fs

// Napisz funkcję, która tworzy plik i zapisuje do niego tekst
fn write_to_file() {
  // Określ nazwę pliku
  let file_name = "moj_plik.txt"

  // Określ treść, którą chcesz zapisać
  let content = "Witaj świecie!"

  // Otwórz plik w trybie "zapisu"
  let file = fs.open(file_name, fs.Write)

  // Zapisz tekst do pliku
  fs.write(file, content)

  // Zamknij plik
  fs.close(file)
}

// Wywołaj funkcję
write_to_file()
```

Po uruchomieniu powyższego kodu, w tym samym katalogu, w którym znajduje się Twój kod programu, zostanie utworzony plik o nazwie "moj_plik.txt", a w nim znajdzie się treść "Witaj świecie!".

## Głębszy zanurzenie

Pisanie plików tekstowych może być również bardziej zaawansowane, gdy zaczynamy używać różnych funkcji i metod, takich jak odczytywanie i edycja istniejących plików, przechwytywanie błędów, czy obsługa różnych formatów tekstu. W celu uzyskania bardziej szczegółowych informacji na temat pisania plików tekstowych w Gleamie, warto zapoznać się z dokumentacją języka lub przeprowadzić dodatkowe eksperymenty i ćwiczenia.

## Zobacz również

- [Dokumentacja języka Gleam](https://gleam.run/)
- [Przykładowy kod Gleam](https://github.com/gleam-lang/)