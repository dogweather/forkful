---
title:                "Iniciando um novo projeto"
date:                  2024-01-20T18:03:39.521493-07:00
model:                 gpt-4-1106-preview
simple_title:         "Iniciando um novo projeto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?
Começar um novo projeto de programação é como plantar uma semente digital; você possui apenas a ideia e o terreno (seu computador). Programadores começam novos projetos para criar soluções, explorar tecnologias, e trazer suas ideias à realidade.

## How to:
Para iniciar um projeto Gleam, você vai precisar do rebar3 e do plugin do Gleam instalados. Aqui está como você os configura e inicia seu projeto:

```gleam
// Instale o plugin do Gleam para o rebar3
$ rebar3 plugins upgrade gleam

// Crie um novo projeto Gleam
$ rebar3 new gleam my_cool_project
```

Esses comandos criam a estrutura básica para o seu projeto Gleam.

## Deep Dive:
Gleam é uma linguagem funcional, tipificada estaticamente, que roda na BEAM, a máquina virtual do Erlang, que é notável pela sua concorrência e tolerância a falhas. Inspirada por linguagens como Rust e Elm, Gleam foi criada para trazer segurança de tipos ao ecossistema do Erlang/Elixir.

Alternativas incluem continuar com Elixir ou Erlang para projetos na BEAM, que têm uma comunidade mais estabelecida, mas Gleam oferece uma experiência mais moderna com tipos estáticos.

Os projetos Gleam se integram bem com bibliotecas Erlang/Elixir, então você pode começar com Gleam enquanto ainda aproveita a vasta gama de pacotes disponíveis para essas linguagens.

## See Also:
- Documentação oficial do Gleam: [https://gleam.run/](https://gleam.run/)
- Tutorial de instalação do Gleam: [https://gleam.run/book/getting-started/installing-gleam.html](https://gleam.run/book/getting-started/installing-gleam.html)
- GitHub do Gleam para exemplos e ferramentas de contribuição: [https://github.com/gleam-lang/gleam](https://github.com/gleam-lang/gleam)
- Comunidade do Gleam no Discord para suporte e discussão: [https://discord.gg/Fm8Pwmy](https://discord.gg/Fm8Pwmy)
