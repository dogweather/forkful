---
title:                "Rust: Читання аргументів командного рядка"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Чому

Програмування мовою Rust є однією з найпопулярніших та ефективних технологій у світі сьогодні. І одним з важливих аспектів програмування є робота з аргументами командного рядка. У цій статті ми розглянемо, чому це важливо та як правильно реалізувати цей процес.

## Як реалізувати

Для реалізації зчитування аргументів командного рядка ми будемо використовувати розширення "clap" (Command Line Argument Parser) для мови Rust. Це дозволяє нам легко та зручно зчитувати та обробляти аргументи з командної стрічки.

```Rust
use clap::{App, Arg};

fn main() {
    let matches = App::new("my_program")
        .version("1.0")
        .author("John Smith")
        .about("A simple program for reading command line arguments")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("FILE")
                .help("Sets the input file")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("output")
                .short("o")
                .long("output")
                .value_name("FILE")
                .help("Sets the output file")
                .takes_value(true),
        )
        .get_matches();

    // Зчитування значень аргументів
    let input_file = matches.value_of("input").unwrap();
    let output_file = matches.value_of("output").unwrap();

    println!("Input file: {}, Output file: {}", input_file, output_file)
}
```

Приклад запуску програми та виведення результату:

```
$ ./my_program -i input.txt -o output.txt
Input file: input.txt, Output file: output.txt
```

## Глибоке дослідження

Як ми бачимо з прикладу, використання розширення "clap" дозволяє нам легко створювати програми, які можуть приймати та обробляти аргументи з командної стрічки. Для більш глибокого розуміння цього процесу, рекомендується ознайомитися з документацією до розширення та додатковими прикладами його використання.

## Дивись також

- [Документація до розширення "clap"](https://docs.rs/clap/2.33.0/clap/)
- [Приклади використання "clap" для реалізації зчитування аргументів командного рядка](https://github.com/clap-rs/clap/tree/v2.33.0/examples)