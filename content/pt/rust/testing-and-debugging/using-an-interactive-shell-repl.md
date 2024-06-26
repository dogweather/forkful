---
date: 2024-01-26 04:18:02.220639-07:00
description: "Como fazer: At\xE9 o momento, Rust n\xE3o possui um REPL oficial integrado.\
  \ Voc\xEA pode usar ferramentas de terceiros como o `evcxr_repl`. Instale-o com\
  \ o Cargo."
lastmod: '2024-03-13T22:44:46.369255-06:00'
model: gpt-4-0125-preview
summary: "At\xE9 o momento, Rust n\xE3o possui um REPL oficial integrado."
title: Usando um shell interativo (REPL)
weight: 34
---

## Como fazer:
Até o momento, Rust não possui um REPL oficial integrado. Você pode usar ferramentas de terceiros como o `evcxr_repl`. Instale-o com o Cargo:

```sh
cargo install evcxr_repl
```

Em seguida, execute o REPL:

```sh
evcxr
```

Dentro dele, teste algum código Rust:

```rust
let x = 5;
let y = 3;
println!("{} + {} = {}", x, y, x + y);
```

O resultado deve ser:

```
5 + 3 = 8
```

## Mergulho Profundo
A filosofia da Rust é centrada em segurança e desempenho, que geralmente são associados com linguagens compiladas antes da execução, e menos com as interpretadas, amigáveis ao REPL. Historicamente, linguagens como Python ou Ruby priorizaram ter um REPL para um feedback imediato, mas não foram projetadas com tarefas em nível de sistema em mente.

Apesar da ausência de um REPL oficial em Rust, surgiram algumas alternativas como o `evcxr_repl`. Estes projetos não estão apenas adaptando Rust para um REPL; eles estão inteligentemente entrelaçando o ciclo de compilação e execução da linguagem em uma sessão interativa. O REPL compila o código por trás dos panos e executa o binário, capturando a saída. Desta forma, ele preserva os benefícios de desempenho de Rust enquanto ainda oferece aquela experiência interativa.

Há uma discussão em andamento na comunidade Rust sobre o suporte oficial ao REPL, e com cada iteração da linguagem, vemos mais sofisticação nas ferramentas que eventualmente podem levar a uma solução nativa.

## Veja Também
Para mais informações e outras ferramentas:
- Repositório GitHub do Evcxr REPL: [https://github.com/google/evcxr](https://github.com/google/evcxr)
- Rust Playground, uma maneira online de testar código Rust: [https://play.rust-lang.org/](https://play.rust-lang.org/)
- Discussão sobre a funcionalidade REPL na linguagem Rust: [https://internals.rust-lang.org/](https://internals.rust-lang.org/)
