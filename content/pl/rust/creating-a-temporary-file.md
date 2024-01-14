---
title:                "Rust: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Każdy programista staje przed wyzwaniem tymczasowego przechowywania danych w trakcie wykonywania programu. W takiej sytuacji bardzo przydatnym rozwiązaniem jest tworzenie plików tymczasowych, które umożliwiają przechowywanie danych na czas działania programu. W tym artykule dowiesz się jak w łatwy sposób tworzyć pliki tymczasowe w języku Rust.

## Jak to zrobić

Tworzenie plików tymczasowych w języku Rust jest bardzo proste i wymaga tylko kilku linijek kodu. Pierwszym krokiem jest zaimportowanie biblioteki "std::fs", która jest odpowiedzialna za operacje na systemie plików. Następnie, używając funkcji "tempfile()", można utworzyć nowy tymczasowy plik i przypisać mu nazwę, na przykład "my_temp_file". W ostatnim kroku wystarczy tylko stworzyć plik wywołując funkcję "create()" i przekazać mu nazwę pliku. Kod w wyglądzie " ```Rust
use std::fs;
let my_temp_file = fs::tempfile("my_temp_file").unwrap();
my_temp_file.create("my_temp_file");
```

Po wykonaniu tego kodu, w katalogu w którym znajduje się program pojawi się nowy plik tymczasowy o nazwie "my_temp_file". Możemy teraz korzystać z tego pliku do przechowywania danych w programie. 

## Głębszy wgląd

Funkcja "tempfile()" pozwala również na określenie opcji takich jak rozmiar pliku, który chcielibyśmy utworzyć. Dzięki temu mamy większą kontrolę nad procesem tworzenia pliku tymczasowego. Warto również pamiętać, że plik tymczasowy zostanie automatycznie usunięty po zakończeniu programu, więc nie musimy martwić się o jego ręczne usuwanie.

## Zobacz również

- Dokumentacja biblioteki "std::fs" (https://doc.rust-lang.org/std/fs/index.html)
- Przykłady użycia funkcji "tempfile()" (https://doc.rust-lang.org/std/fs/fn.tempfile.html)
- Instrukcja tworzenia plików w języku Rust (https://www.rust-lang.org/learn/get-started#creating-a-new-project-and-building-a-binary)