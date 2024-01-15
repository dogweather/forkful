---
title:                "Odczytywanie pliku tekstowego"
html_title:           "Rust: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś zainteresowany językiem programowania Rust, prawdopodobnie chcesz nauczyć się, jak czytać pliki tekstowe w tym języku. Jest to bardzo przydatna umiejętność, ponieważ wiele aplikacji i narzędzi, takich jak bazy danych, wykorzystuje pliki tekstowe jako format przechowywania danych. W tym artykule dowiesz się, jak w łatwy sposób czytać pliki tekstowe w Rust.

## Jak to zrobić

Aby móc czytać pliki tekstowe w Rust, musisz najpierw zaimportować moduł "fs" (filesystem). Możesz to zrobić używając słowa kluczowego "use" i podając nazwę modułu, w tym przypadku "fs". Następnie możesz użyć funkcji "fs::read_to_string", aby odczytać zawartość pliku tekstowego i zachować ją w zmiennej. 

```Rust
use std::fs;

let tekst = fs::read_to_string("plik.txt").expect("Nie udało się odczytać pliku!");
```

W powyższym przykładzie, wykorzystaliśmy funkcję "expect" w celu obsługi wyjątków. Jeśli wystąpił błąd podczas odczytu pliku, zostanie wyświetlony komunikat "Nie udało się odczytać pliku!". Możesz również użyć funkcji "unwrap", która zawiera w sobie błąd i automatycznie go obsługuje.

Aby odczytać pojedynczą linię z pliku, możesz użyć funkcji "read_line" i zachować wynik w zmiennej typu String.

```Rust
let mut linia = String::new();
let wynik = fs::File::open("plik.txt").expect("Nie udało się otworzyć pliku!");

wynik.read_line(&mut linia).expect("Nie udało się odczytać linii.");
```

Jeśli chcesz odczytać całą zawartość pliku w celu przetworzenia danych, możesz użyć pętli for i funkcji "lines".

```Rust
let wynik = fs::File::open("plik.txt").expect("Nie udało się otworzyć pliku!");

for linia in wynik.lines() {
    //przetworzenie danych
}
```

## Deep Dive

W powyższych przykładach wykorzystaliśmy funkcję "expect", która jest dostępna w każdym typie zwracanym przez funkcje biblioteki standardowej. Jednak w niektórych przypadkach może to powodować problemy i warto jest użyć funkcji "unwrap", która zwraca wynik lub wyrzuca błąd.

Podczas przetwarzania plików tekstowych, często zdarza się, że chcesz wykluczyć pewne części tekstu lub pominąć linie, które nie zawierają potrzebnych informacji. W tym celu możesz skorzystać z funkcji "filter" i "find". Funkcja "filter" pozwala na wykluczenie całych linii zawierających określony ciąg znaków, natomiast funkcja "find" pozwala na znalezienie kawałka tekstu lub wyrażenia regularnego w linii.

Na koniec chciałbym wspomnieć o funkcji "fs::metadata", która zwraca informacje o pliku, takie jak rozmiar, data ostatniej modyfikacji, czy też flagi dostępu. Używając tej funkcji, możesz łatwo sprawdzić, czy plik jest pusty i czy nie został ostatnio zmodyfikowany.

## Zobacz też

- Dokumentacja Rust na temat czytania i zapisywania plików: https://doc.rust-lang.org/std/fs/index.html
- Przykłady kodu na GitHubie: https://github.com/rust-lang/rust/tree/master/src/libstd/fs.rs
- Poradnik na YouTube: https://www.youtube.com/watch?v=KJOkRG4-uMA