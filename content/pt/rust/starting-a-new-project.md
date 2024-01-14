---
title:                "Rust: Iniciando um novo projeto"
simple_title:         "Iniciando um novo projeto"
programming_language: "Rust"
category:             "Rust"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por que iniciar um novo projeto em Rust?

Rust tem se tornado uma linguagem muito popular entre os desenvolvedores por sua segurança, performance e facilidade de manutenção. Além disso, sua comunidade ativa e recursos excelentes tornam Rust uma ótima opção para iniciar um novo projeto.

## Como fazer?

Para iniciar um novo projeto em Rust, basta seguir estes passos:

1. Instale o compilador Rust em sua máquina, o qual pode ser encontrado no site oficial da linguagem.
2. Crie uma nova pasta para o seu projeto e navegue até ela no terminal.
3. Inicie um novo projeto com o comando `cargo new nome_do_projeto`.
4. O Cargo, gerenciador de pacotes do Rust, irá criar uma estrutura básica para seu projeto, incluindo um arquivo `main.rs` inicial.
5. Agora, basta abrir o arquivo `main.rs` em um editor de código e começar a codificar seu projeto em Rust!

Aqui está um exemplo simples de código em Rust, que imprime a clássica frase "Hello, world!" na tela:

```Rust
fn main() {
    println!("Hello, world!");
}
```

E a saída seria:

```
Hello, world!
```

Para executar este código, basta digitar `cargo run` no seu terminal dentro da pasta do projeto.

## Aprofundando-se

Para iniciar um projeto em Rust de forma mais avançada, é importante entender os conceitos básicos da linguagem, como tipos de dados, estruturas de controle e funções. Além disso, é recomendado explorar bibliotecas e frameworks disponíveis para facilitar o desenvolvimento do projeto.

Uma ótima forma de se aprofundar em Rust é através de leituras de livros, documentações e tutoriais online. Alguns recursos recomendados são:

- [The Rust Programming Language](https://doc.rust-lang.org/book/)
- [Rust by Example](https://doc.rust-lang.org/rust-by-example/)
- [Rust Cookbook](https://rust-lang-nursery.github.io/rust-cookbook/)

## Veja também

- [Site oficial do Rust](https://www.rust-lang.org/)
- [Repositório oficial do Cargo](https://github.com/rust-lang/cargo)
- [Crates.io, repositório de bibliotecas do Rust](https://crates.io/)