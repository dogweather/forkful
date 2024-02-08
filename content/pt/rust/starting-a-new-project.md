---
title:                "Iniciando um novo projeto"
aliases:
- pt/rust/starting-a-new-project.md
date:                  2024-01-20T18:04:21.806036-07:00
model:                 gpt-4-1106-preview
simple_title:         "Iniciando um novo projeto"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Iniciar um novo projeto em Rust é como desenhar o molde para uma nova invenção. Programadores fazem isso para transformar ideias em código, criando a base para aplicações robustas e seguras.

## Como fazer:

Para começar um projeto novo em Rust, você vai utilizar o `cargo`, o sistema de construção e gerenciador de pacotes do Rust. Aqui está o caminho:

```Rust
// Na linha de comando, digite:
cargo new meu_projeto

// Veja a estrutura criada:
cd meu_projeto
tree
```

Saída esperada (pode variar conforme seu sistema):

```
meu_projeto
├── Cargo.toml
└── src
    └── main.rs
```

Isso cria um novo diretório chamado `meu_projeto` com uma configuração inicial:

- `Cargo.toml`: O manifesto do seu projeto com metadados e dependências.
- `src/main.rs`: O ponto de entrada do seu código, com uma função `main`.

Teste o código gerado:

```Rust
// Compile e execute seu projeto:
cargo run
```

Saída esperada:

```
   Compiling meu_projeto v0.1.0 (/path/to/meu_projeto)
    Finished dev [unoptimized + debuginfo] target(s) in 0.0 secs
     Running `target/debug/meu_projeto`
Hello, world!
```

## Aprofundamento

Rust surgiu em 2010 pela mão de Graydon Hoare e cresceu com a ajuda da Mozilla. Diferentemente de C++ e outras linguagens de baixo nível, Rust enfatiza a segurança de memória e concorrência sem sacrificar a performance.

Alternativas existem, como criar projetos manualmente, mas são mais suscetíveis a erros e menos práticos. `Cargo` automatiza e padroniza esse processo. Quando você usa `cargo new`, o Rust configura tudo para o manejo correto de dependências, builds e testes, estabelecendo um ambiente pronto para a programação.

Detalhes a reter:

- `Cargo.toml` usa TOML (Tom's Obvious, Minimal Language), fácil de ler e escrever.
- O código fonte vai na pasta `src`; `main.rs` é especial pois é onde o ponto de entrada (`main` function) deve estar se você está construindo um binário.
- O sistema de módulos do Rust ajuda a organizar código à medida que seu projeto cresce.

## Veja Também

- [The Rust Programming Language Book](https://doc.rust-lang.org/book/)
- [`cargo` documentation](https://doc.rust-lang.org/cargo/)
- [Rust and WebAssembly](https://rustwasm.github.io/docs.html)
- [Rust by Example](https://doc.rust-lang.org/rust-by-example/)
