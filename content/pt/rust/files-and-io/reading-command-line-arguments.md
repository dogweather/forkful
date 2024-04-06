---
date: 2024-01-20 17:57:02.185620-07:00
description: "Como Fazer: Execute o programa com `cargo run` seguido pelos argumentos.\
  \ Aqui est\xE1 um exemplo de output."
lastmod: '2024-04-05T21:53:46.715977-06:00'
model: gpt-4-1106-preview
summary: Execute o programa com `cargo run` seguido pelos argumentos.
title: Lendo argumentos da linha de comando
weight: 23
---

## Como Fazer:
```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 1 {
        println!("Olá, {}!", args[1]);
    } else {
        println!("Oi! Tente passar um nome como argumento.");
    }
}
```
Execute o programa com `cargo run` seguido pelos argumentos. Aqui está um exemplo de output:

```
$ cargo run Fulano
Olá, Fulano!
```

## Mergulho Profundo
Historicamente, a maneira de acessar argumentos de linha de comando tem sido através de interfaces específicas do sistema operacional. Em Rust, a biblioteca padrão `std::env` simplifica esse processo com funções como `args()`, que retorna um iterador dos argumentos. Alternativamente, existem crates como `clap` e `structopt` que oferecem funcionalidades mais avançadas para parsing de argumentos. Internamente, quando você chama `env::args()`, o Rust lida com as peculiaridades de cada sistema operacional para fornecer uma interface consistente.

## Veja Também
- Documentação oficial de Rust sobre `std::env`: https://doc.rust-lang.org/std/env/
- Crates para gestão de argumentos de linha de comando:
  - `clap`: https://crates.io/crates/clap
  - `structopt`: https://crates.io/crates/structopt
- Artigo sobre parsing de argumentos de linha de comando com Rust: https://rust-lang-nursery.github.io/cli-wg/tutorial/cli-args.html
