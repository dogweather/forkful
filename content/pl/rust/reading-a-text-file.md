---
title:                "Rust: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, jak programy komputerowe są w stanie czytać pliki tekstowe? W tym krótkim wpisie przeczytasz o tym, dlaczego warto poznać koncepcję czytania plików tekstowych w języku Rust.

## Jak to zrobić?

Poniżej przedstawiamy prosty przykład kodu w języku Rust, który umożliwia odczytanie pliku tekstowego i wyświetlenie jego zawartości w konsoli.

```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    // Otwórz plik tekstowy
    let mut file = File::open("plik.txt").expect("Nie można otworzyć pliku.");

    // Utwórz bufor do odczytania pliku
    let mut contents = String::new();

    // Odczytaj zawartość pliku do bufora
    file.read_to_string(&mut contents).expect("Nie można odczytać pliku.");

    // Wyświetl zawartość pliku w konsoli
    println!("{}", contents);
}
```

Przykładowy plik "plik.txt" może zawierać taką treść:

```
Witaj, świecie!
```

A po uruchomieniu kodu, powinniśmy zobaczyć taki output w konsoli:

```
Witaj, świecie!
```

## Głębsza analiza

Teraz skupimy się na wyjaśnieniu niektórych kluczowych elementów z przykładowego kodu. W pierwszej linijce wykorzystujemy moduł `fs` z biblioteki standardowej Rusta, aby zaimportować funkcję `File`, która pozwala na otwieranie plików. Następnie, w trzeciej linijce, używamy funkcji `expect` w celu obsłużenia błędów - w przypadku, gdy nie uda się otworzyć pliku, program zwróci nam wiadomość `Nie można otworzyć pliku.`

W linijce piątej tworzymy bufor danych, który zostanie wykorzystany do przechowywania zawartości pliku. Następnie, w szóstej linijce wywołujemy funkcję `read_to_string`, która odczytuje zawartość pliku i zapisuje ją do bufora. W przypadku, gdy wystąpi błąd podczas odczytywania, program zwróci nam wiadomość `Nie można odczytać pliku.`

Na koniec, w linii ósmej, wyświetlamy zawartość bufora w konsoli za pomocą funkcji `println!`, która jest częścią makra standardowego.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o programowaniu w języku Rust, warto przejrzeć inne wpisy na naszym blogu oraz odwiedzić następujące strony:

- [Oficjalna strona języka Rust](https://www.rust-lang.org/)
- [Dokumentacja Rusta](https://doc.rust-lang.org/book/)
- [Rust by Example - przykłady kodu w języku Rust](https://doc.rust-lang.org/stable/rust-by-example/)
- [Reddit Rust - popularne forum dla społeczności języka Rust](https://www.reddit.com/r/rust/)