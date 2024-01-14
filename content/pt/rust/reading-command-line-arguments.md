---
title:    "Rust: Lendo argumentos da linha de comando"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando em Rust?

A leitura de argumentos da linha de comando é uma habilidade essencial para qualquer programador Rust. Com essa técnica, é possível que nossos programas recebam informações do usuário no momento em que são executados, tornando-os mais dinâmicos e interativos. Se você está aprendendo Rust ou já programa nesta linguagem, é importante entender como ler argumentos da linha de comando para aprimorar suas habilidades.

## Como ler argumentos da linha de comando em Rust

Ler argumentos da linha de comando em Rust é bastante simples. Primeiramente, precisamos importar a biblioteca `std::env`, que nos fornece funções para acessar os argumentos passados para o programa. Em seguida, utilizamos a função `args()` para obter um iterador contendo os argumentos.

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    // restante do código para processar os argumentos
}
```

Com esse iterador, podemos acessar cada argumento individualmente utilizando métodos como `next()` e `unwrap()`. Por exemplo, se quisermos imprimir o primeiro argumento passado pelo usuário, podemos utilizar o seguinte código:

```Rust
println!("Primeiro argumento: {}", args[1]);
```

Além disso, também podemos utilizar a função `len()` para verificar se foram fornecidos argumentos suficientes e garantir que não ocorram erros.

## Aprofundando no assunto

Além de ler argumentos passados pelo usuário, também podemos fornecê-los manualmente através do arquivo `Cargo.toml`. Por exemplo, no arquivo de configuração do seu projeto, você pode adicionar a seguinte linha:

```
[package]
name = "meu_projeto"
version = "0.1.0"
authors = ["Seu Nome <seu@email>"]
description = "Meu novo projeto"
edition = "2018"

[dependencies]
rand = "0.6.0"

[package.metadata.rustlearners.arguments]
nome = "Rust Learners"
```

Assim, ao executar o comando `cargo run -- nome`, o valor fornecido para `nome` será passado como argumento para o programa e pode ser acessado da mesma forma que os argumentos da linha de comando.

## Veja também

- [Documentação oficial do Rust sobre a biblioteca std::env](https://doc.rust-lang.org/std/env/index.html)
- [Tutorial sobre leitura de argumentos da linha de comando em Rust](https://www.youtube.com/watch?v=x5ovZkvCO-0)
- [Exemplo prático de leitura de argumentos da linha de comando em Rust](https://gist.github.com/seppo0010/ab10c41634bdfbeeeba4)