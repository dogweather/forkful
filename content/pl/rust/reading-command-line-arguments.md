---
title:                "Czytanie argumentów linii poleceń"
html_title:           "Bash: Czytanie argumentów linii poleceń"
simple_title:         "Czytanie argumentów linii poleceń"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Czytanie argumentów z linii poleceń to proces, w którym program komputerowy odczytuje dane wprowadzone przez użytkownika przez terminal. Programiści robią to, aby użytkownik mógł dostosować działanie programu bez konieczności modyfikowania jego kodu źródłowego.

## Jak to zrobić:

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    println!("You gave {:?} arguments.", args.len());
    for (index, argument) in args.iter().enumerate() {
        println!("Argument {} was: {}", index, argument);
    }
}
```

Po uruchomieniu tego kodu z kilkoma argumentami, na przykład `program arg1 arg2 arg3`, otrzymasz następujące wyniki:

```Rust
You gave 4 arguments.
Argument 0 was: program
Argument 1 was: arg1
Argument 2 was: arg2
Argument 3 was: arg3
```

## Deep Dive

Czytanie argumentów linii poleceń to praktyka, która sięga początków programowania. W języku Rust przejmowanie argumentów poleceń odbywa się za pomocą funkcji `env::args()` z biblioteki standardowej. Alternatywą mogą być biblioteki zewnętrzne, takie jak `getopts` czy `clap`, które oferują bardziej rozbudowane opcje. Główny mechanizm pozostaje jednak taki sam: mamy listę argumentów, które są przekazane jako ciągi znaków, które następnie mogą być przetworzone według potrzeb.

## Zobacz też:

- [Dokumentacja języka Rust na temat `std::env`](https://doc.rust-lang.org/std/env/)
- [Biblioteka `getopts` dla Rust](https://docs.rs/getopts/0.2.21/getopts/)
- [Biblioteka `clap` dla Rust](https://docs.rs/clap/2.33.3/clap/)