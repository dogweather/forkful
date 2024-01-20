---
title:                "Lendo argumentos de linha de comando"
html_title:           "Arduino: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O quê & Por quê?
A leitura de argumentos de linha de comando é um processo pelo qual um programa obtém dados de entrada da linha de comando. Isso é feito para permitir a personalização do comportamento de um programa no momento da execução.

## Como fazer:
Em Rust, a biblioteca padrão fornece o módulo `std::env` que pode ser usado para acessar argumentos de linha de comando. Aqui está um exemplo de código:

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("{:?}", args);
}
```
Se você passar `"Hello"` `"World"` como argumentos de linha de comando para este programa, a saída será:

```Rust
["path_to_program", "Hello", "World"]
```
...indicando que o primeiro argumento é sempre o caminho para o próprio programa.

## Mergulho profundo
- **Contexto histórico**: A prática de ler argumentos da linha de comando remonta aos primeiros dias de programação, quando a interação com o usuário era frequentemente feita via linha de comando.
- **Alternativas**: Em Rust, também podemos usar bibliotecas de terceiros como `clap` ou `getopts` para lidar com argumentos de linha de comando, fornecendo recursos adicionais e sintaxe mais agradável.
- **Detalhes de implementação**: `std::env::args()` retorna um iterador que podemos coletar em um `Vec<String>`. Cada item no `Vec` é um argumento de linha de comando individual, incluindo o nome do próprio programa como o primeiro item.

## Veja também
- [Documentação `std::env::args()`](https://doc.rust-lang.org/std/env/fn.args.html)
- [Biblioteca clap](https://clap.rs)
- [Biblioteca getopts](https://docs.rs/getopts/0.2.21/getopts/)