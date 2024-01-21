---
title:                "Odczytywanie argumentów linii poleceń"
date:                  2024-01-20T17:57:06.134737-07:00
model:                 gpt-4-1106-preview
simple_title:         "Odczytywanie argumentów linii poleceń"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Czytanie argumentów linii poleceń to pobieranie danych wprowadzonych przez użytkownika podczas wywoływania programu. Programiści używają tego, by umożliwić użytkownikom dostosowywanie działania aplikacji na potrzeby chwili.

## Jak to zrobić:
W Rust używamy crate`a `std::env` do obsługi argumentów:

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    println!("Otrzymane argumenty:");
    for arg in args.iter() {
        println!("{}", arg);
    }
}
```

Uruchomienie tego kodu z argumentami:
```
$ cargo run arg1 arg2 arg3
```

Wypisze na ekranie:
```
Otrzymane argumenty:
ścieżka/do/programu
arg1
arg2
arg3
```

## Deep Dive
Historia czytania argumentów linii poleceń sięga wczesnych dni informatyki, gdy interfejsy tekstowe były standardem. W Rust, oprócz `std::env`, mamy potężne crate'y takie jak `clap` czy `structopt`, które pozwalają definiować argumenty w sposób deklaratywny i pozwala na łatwą walidację oraz dokumentację.

Implementacja w `std::env` jest dosyć nisko poziomowa i bezpośrednia – dostajemy dokładnie to, co zostało wpisane przy uruchomieniu. Funkcja `env::args()` zwraca iterator, który pozwala iterować po argumentach.

## Zobacz również
- Oficjalna dokumentacja Rusta o `std::env`: https://doc.rust-lang.org/std/env/
- Repozytorium crate `clap`: https://github.com/clap-rs/clap
- Repozytorium crate `structopt`: https://github.com/TeXitoi/structopt
- Rust by Example na temat argumentów linii poleceń: https://doc.rust-lang.org/rust-by-example/std_misc/arg.html