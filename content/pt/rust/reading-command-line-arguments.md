---
title:                "Rust: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando

Ao escrever programas em Rust, muitas vezes é necessário ler os argumentos fornecidos pelo usuário na linha de comando. Isso permite que o programa seja executado de maneira diferente dependendo dos argumentos inseridos, tornando-o mais versátil e útil para os usuários.

## Como fazer

Para ler argumentos da linha de comando em Rust, primeiro é necessário importar a biblioteca `std::env`. Em seguida, podemos usar a função `args()` para recuperar uma lista dos argumentos passados para o programa. Aqui está um exemplo de como isso é feito:

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("{:?}", args);
}
```

Neste exemplo, a variável `args` irá conter uma lista de Strings contendo os argumentos fornecidos pelo usuário. Podemos então usá-la para executar diferentes operações com base nos argumentos inseridos.

## Mergulho profundo

Para uma exploração mais aprofundada, podemos utilizar o pacote `clap` para facilitar a leitura de argumentos da linha de comando em Rust. O `clap` é uma biblioteca de análise de linha de comando que nos permite definir facilmente opções, argumentos e flag para o nosso programa. Aqui está um exemplo de como usá-lo:

```Rust
use clap::{App, Arg};

fn main() {
    let matches = App::new("Meu Programa")
        .version("1.0")
        .author("Meu Nome")
        .about("Este programa lê argumentos da linha de comando")
        .arg(Arg::with_name("arquivo")
             .short("f")
             .long("file")
             .value_name("ARQUIVO")
             .help("Define o arquivo a ser lido")
             .takes_value(true))
        .get_matches();

    // Usando o argumento fornecido pelo usuário
    let arquivo = matches.value_of("arquivo").unwrap_or("config.txt");

    println!("Lendo o arquivo: {}", arquivo);
}
```

Este é apenas um exemplo simples do uso do `clap` para facilitar a leitura de argumentos da linha de comando. Para mais informações e opções avançadas, consulte a documentação oficial do `clap`.

## Veja também

- [Documentação oficial do `std::env`](https://doc.rust-lang.org/std/env/index.html)
- [Documentação oficial do `clap`](https://docs.rs/clap/2.33.0/clap/)
- [Tutorial sobre como ler argumentos da linha de comando em Rust](https://www.it-swarm.net/pt/rust/como-posso-implementar-a-análise-de-linha-de-comandos-em-rust/940772095/)