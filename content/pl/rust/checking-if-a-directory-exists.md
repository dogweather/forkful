---
title:    "Rust: Sprawdzanie czy istnieje katalog"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Istnienie katalogu jest ważnym aspektem każdego programu, który musi wykonywać operacje na plikach. Właściwe sprawdzanie, czy dany katalog istnieje, pomaga uniknąć błędów i nieporozumień, a także ułatwia utrzymanie poprawnej struktury plików w systemie. W tym artykule dowiesz się, jak w łatwy sposób można sprawdzić istnienie katalogu w języku Rust.

## Jak to zrobić

Rust oferuje proste i intuicyjne rozwiązanie do sprawdzania istnienia katalogu - moduł `std::fs`. Aby skorzystać z tego modułu, należy najpierw zaimportować go do swojego programu:

```Rust
use std::fs;
```

Następnie, aby sprawdzić, czy dany katalog istnieje, należy użyć funkcji `metadata` z tego modułu i przekazać jako argument nazwę katalogu, który chcemy sprawdzić:

```Rust
let path = "sciezka/do/katalogu";
let metadata = fs::metadata(path);
```

Funkcja `metadata` zwraca wartość typu `Result`, która może zawierać informacje o katalogu, jeśli istnieje, lub błąd, jeśli nie. Aby sprawdzić, czy katalog istnieje, należy wywołać funkcję `is_dir` na obiekcie `metadata`:

```Rust
if metadata.is_ok() && metadata.unwrap().is_dir() {
    println!("Katalog istnieje!");
} else {
    println!("Katalog nie istnieje!");
}
```

## Również warto wiedzieć

Ponadto, istnieje wiele innych funkcji z modułu `std::fs`, które mogą być przydatne przy pracy z katalogami, np. `create_dir` (tworzy nowy katalog), `remove_dir` (usuwa katalog) czy `read_dir` (odczytuje zawartość katalogu). Więcej informacji na ten temat znajdziesz w dokumentacji języka Rust i w przykładach kodu.

## Zobacz także (See Also)

- Dokumentacja oficjalna języka Rust: https://doc.rust-lang.org/std/fs/index.html
- Przykładowy kod sprawdzający istnienie katalogu w Rust Playground: https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=2c697daf91fa1a440ec4c26ef0c319c6