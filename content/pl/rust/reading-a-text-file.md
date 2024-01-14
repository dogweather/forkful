---
title:                "Rust: Odczytywanie pliku tekstowego"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego
Czy kiedykolwiek chciałeś/chciałaś przeczytać plik tekstowy w swoim programie Rust? Może chcesz stworzyć aplikację do przetwarzania tekstu lub analizy danych. Ten artykuł pokaże Ci, jak to zrobić w prosty sposób.

## Jak to zrobić
Aby przeczytać plik tekstowy w Rust, możesz użyć wbudowanej biblioteki `std::fs`. Najpierw musisz otworzyć plik przy użyciu funkcji `File::open()` i przekazać jako argument ścieżkę pliku. Następnie możesz odczytać zawartość pliku przy użyciu metody `read_to_string()` i zapisać ją do zmiennej. Poniżej znajdziesz przykładowy kod:

```Rust
use std::fs;
use std::io::Read;

fn main() {
    let file_name = "sample.txt";
    let mut file = fs::File::open(file_name).expect("Błąd podczas otwierania pliku");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("Błąd podczas czytania pliku");
    println!("{}", contents);
}
```

Wyjście dla pliku `sample.txt` zawierającego tekst "Cześć, jestem przykładowym tekstem" będzie wyglądać następująco:

```
Cześć, jestem przykładowym tekstem
```

## Głębszy zanurzenie
Metoda `read_to_string()` jest przydatna, ale może być niewydajna dla dużych plików. Istnieje też wiele innych metod odczytu plików, takich jak `read()` i `lines()`, które pozwalają na bardziej elastyczne i wydajne przetwarzanie plików tekstowych. Możesz również użyć biblioteki zewnętrznej, takiej jak `csv` lub `regex`, aby przetworzyć dane odczytane z pliku.

## Zobacz również
- Dokumentacja Rust na temat czytania plików: https://doc.rust-lang.org/std/fs/struct.File.html
- Przykładowe programy demonstrujące czytanie plików w Rust: https://github.com/pretzelhammer/rust-blog/blob/master/posts/reading-files-in-rust.md
- Biblioteka `csv` dla przetwarzania plików CSV w Rust: https://docs.rs/csv/1.1.1/csv/
- Biblioteka `regex` dla przetwarzania tekstu przy użyciu wyrażeń regularnych w Rust: https://docs.rs/regex/1.0.3/regex/