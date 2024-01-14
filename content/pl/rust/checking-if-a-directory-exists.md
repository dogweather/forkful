---
title:    "Rust: Sprawdzanie czy katalog istnieje"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Dlaczego?

Sprawdzanie istnienia katalogu jest ważnym elementem programowania, ponieważ pozwala nam na wykonywanie różnych akcji w zależności od tego, czy dany katalog istnieje czy nie. To przydatna umiejętność, którą warto poznać w języku Rust.

## Jak to zrobić?

Sprawdzenie istnienia katalogu w języku Rust jest bardzo proste. Wystarczy użyć funkcji `Path::exists()` na instancji struktury `Path`, która reprezentuje nasz katalog. Poniżej znajduje się przykładowy kod:

```rust
use std::fs;

fn main() {
  let directory = Path::new("katalog/");
    
  if directory.exists() {
      println!("Katalog istnieje!");
  } else {
      println!("Katalog nie istnieje!");
  }
}
```

Powyższy kod wykorzystuje bibliotekę standardową `std::fs`, która zawiera funkcje i metody do pracy z plikami i katalogami. Najpierw musimy utworzyć instancję `Path`, wskazującą na nazwę katalogu, którą chcemy sprawdzić. Następnie przy użyciu metody `exists()` sprawdzamy, czy katalog istnieje. W zależności od wyniku, wyświetlamy odpowiedni komunikat.

## Dogłębna analiza

Aby móc sprawdzić, czy katalog istnieje, musimy najpierw ustalić ścieżkę do tego katalogu. W języku Rust, ścieżki są reprezentowane przez struktury `Path` i `PathBuf`. Pierwsza z nich jest immutable (niemodyfikowalna), co oznacza, że ​​nie możemy zmienić ścieżki, która już istnieje. Natomiast `PathBuf` jest mutable (modyfikowalna) i możemy dodawać do niej nowe elementy ścieżki.

Warto również zauważyć, że funkcja `exists()` zwraca wartość typu `bool`, co oznacza, że ​​jest to zmienna logiczna przyjmująca wartość `true` lub `false`. Dzięki temu możemy wykonać pewne działania w zależności od tego, czy katalog istnieje czy nie, np. utworzyć nowy katalog lub usunąć istniejący.

## Zobacz także

* [Dokumentacja Rust - ścieżki](https://doc.rust-lang.org/std/path/)
* [Funkcja exists() w Rust](https://docs.rs/exists/0.1.0/exists/)
* [Porównanie aktualnych i niedawnych elementów, czy katalog istnieje w Rust](https://stackoverflow.com/questions/27599725/how-do-i-check-if-a-file-or-directory-exists-in-rust)