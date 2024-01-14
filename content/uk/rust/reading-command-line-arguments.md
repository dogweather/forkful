---
title:    "Rust: Читання аргументів командного рядка"
keywords: ["Rust"]
---

{{< edit_this_page >}}

# Чому

Перевагами командного рядка у програмуванні є швидкість, ефективність і можливість робити зміни безпосередньо з коду. Читання аргументів командного рядка дозволяє програмі отримати інформацію безпосередньо від користувача, що робить її більш гнучкою і зручною.

# Як це зробити

Один із способів читання аргументів командного рядка у мові програмування Rust - використати бібліотеку `std::env`.

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    for arg in args {
        println!("{}", arg); // виводимо всі аргументи в консоль
    }
}
```

Виконання цього коду з такими аргументами:

`./program arg1 arg2 arg3`

дасть нам наступний вивід:

```
./program
arg1
arg2
arg3
```

Це означає, що ми успішно отримали всі передані аргументи.

# Глибокий занурення

У мові Rust, є кілька вбудованих функцій для читання аргументів командного рядка, таких як `env::args()`, `env::args_os()` та `env::args_os()`.

Також, можна використовувати пакети з функціями розбору аргументів командного рядка, які простіше у використанні та дозволяють задавати флаги для аргументів.

Наприклад, пакет `clap` дозволяє нам зробити наступне:

```Rust
use clap::{Arg, App};

fn main() {
    let matches = App::new("My Program")
                    .arg(Arg::with_name("input")
                        .short("i")
                        .long("input")
                        .takes_value(true)
                        .required(true))
                    .arg(Arg::with_name("output")
                        .short("o")
                        .long("output")
                        .takes_value(true)
                        .required(true))
                    .get_matches();

    let input = matches.value_of("input").unwrap();
    let output = matches.value_of("output").unwrap();

    println!("Input file: {}", input);
    println!("Output file: {}", output);
}
```

Цей код дозволяє нам вказувати аргументи за допомогою флагів `-i` або `--input` для вхідного файлу та `-o` або `--output` для вихідного файлу. Також, вказані аргументи будуть обов'язкові.

# Дивись також

- [Документація по бібліотеці std::env у Rust](https://doc.rust-lang.org/std/env/index.html)
- [Пакет clap у Crates.io](https://crates.io/crates/clap)
- [Приклади використання командного рядка у мові Rust](https://rust-cli.github.io/book/index.html?ref=awesome-cli-apps.com)