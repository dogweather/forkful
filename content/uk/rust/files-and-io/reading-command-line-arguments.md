---
title:                "Читання аргументів командного рядка"
aliases:
- /uk/rust/reading-command-line-arguments.md
date:                  2024-01-20T17:57:08.640435-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання аргументів командного рядка"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Що і чому?
Читання аргументів командного рядка – це процес отримання даних, які користувач передає вашій програмі при її запуску. Програмісти використовують це для забезпечення гнучкості і налаштувань в програмах, дозволяючи користувачам вказувати опції та вхідні дані без зміни коду.

## Як це робити:
```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    if args.len() > 1 {
        println!("Your first argument is: {}", args[1]);
    } else {
        println!("No arguments provided!");
    }
}
```
Якщо запустити `program "Hello, world!"`, результат буде:
```
Your first argument is: Hello, world!
```
Якщо запустити просто `program`, результат буде:
```
No arguments provided!
```

## Поглиблений аналіз
Розуміння аргументів командного рядка йшло ще з часів UNIX. В Rust, метод `std::env::args` повертає ітератор аргументів. Існують альтернативи стандартній бібліотеці, наприклад, `clap` і `structopt`, що надають більше можливостей для аналізу аргументів і налаштування параметрів командного рядка. Система типів Rust і патерн `match` можуть бути використані для більш безпечної обробки аргументів.

## Див. також:
- The Rust Programming Language Book, Command Line Arguments: https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html
- Crates `clap` for argument parsing: https://crates.io/crates/clap
- Crates `structopt` for defining options with structs: https://crates.io/crates/structopt
