---
title:                "Iniciando um novo projeto"
html_title:           "Javascript: Iniciando um novo projeto"
simple_title:         "Iniciando um novo projeto"
programming_language: "Rust"
category:             "Rust"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Começar um novo projeto em Rust: Opa! Como que faz?

## O que & Por quê?
Iniciar um novo projeto é literalmente começar a criar um programa do zero. Fazemos isso para atender a um requisito específico que nenhum software existente é capaz de atender, ou simplesmente pelo amor à programação.

## Como Fazer:
Para iniciar um novo projeto em Rust, você precisa ter o gerenciador de pacotes "cargo" instalado. Vamos lá!

```Rust
$ cargo new meu_projeto
$ cd meu_projeto
``` 
Depois disso, a estrutura de pastas do seu novo projeto será assim:

```Rust
meu_projeto/
  .gitignore
  Cargo.toml
  src/
    main.rs
```

Seu programa "Hello, World!" já está pronto para funcionar:

```Rust
$ cargo run
   Compiling meu_projeto v0.1.0 (file:///path/to/meu/projeto)
    Finished dev [unoptimized + debuginfo] target(s) in 2.21s
     Running `target/debug/meu_projeto`
Hello, World!
```

## Mergulho Profundo
1. **Contexto Histórico:** O Rust surgiu para solucionar problemas comuns em outros sistemas de linguagem, como gerenciamento de memória e concorrência. Ao iniciar um novo projeto usando Rust, você terá menos chances de encontrar bugs relacionados a memória.

2. **Alternativas:** Você pode criar projetos em qualquer linguagem, como C++, Java, Python, etc. Pero, Rust oferece controle de baixo nível com segurança de alto nível.

3. **Detalhes da Implementação:** Ao iniciar um novo projeto, 'cargo' configura tudo para você. O arquivo 'Cargo.toml' gerado terá todas as dependências do seu projeto. O 'main.rs' é onde seu código realmente reside.

## Veja Também
- Documentação oficial do Rust: https://doc.rust-lang.org/book/
- Aprenda Rust pelo Exercism.io: https://exercism.io/tracks/rust
- Fonte "incrivelmente fácil" de Rust: https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-misconceptions.html