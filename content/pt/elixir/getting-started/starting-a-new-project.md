---
date: 2024-01-20 18:03:30.088287-07:00
description: "Como fazer: Primeiro, instale o Elixir e a ferramenta de linha de comando\
  \ `mix`. Ent\xE3o, crie seu projeto."
lastmod: '2024-03-13T22:44:46.238837-06:00'
model: gpt-4-1106-preview
summary: Primeiro, instale o Elixir e a ferramenta de linha de comando `mix`.
title: Iniciando um novo projeto
weight: 1
---

## Como fazer:
Primeiro, instale o Elixir e a ferramenta de linha de comando `mix`. Então, crie seu projeto:

```elixir
mix new meu_projeto
```

Isso vai gerar a estrutura básica de diretórios e arquivos. Veja o que acontece:

```elixir
$ mix new meu_projeto
* creating README.md
* creating .formatter.exs
* creating .gitignore
* creating mix.exs
* creating lib
* creating lib/meu_projeto.ex
* creating test
* creating test/test_helper.exs
* creating test/meu_projeto_test.exs

Your Mix project was created successfully.
You can use "mix compile" to compile your project,
"use "iex -S mix" to launch IEx,
"mix test" to run the tests.
```

Pronto, você tem um projeto novo pra brincar!

## Mergulho Profundo:
O `mix` é uma ferramenta poderosa no ecossistema Elixir que gerencia tarefas como compilar o código, rodar testes e gerenciar dependências. Ele vem da época quando José Valim e a comunidade estavam criando Elixir, inspirados por ferramentas de outras linguagens como `bundler`, `lein` e `rebar`.

Alternativas? Bom, no universo Elixir não tem muita opção além do `mix` porque ele é muito enraizado na cultura da linguagem.

Detalhes de implementação: Quando você roda `mix new`, está invocando uma tarefa do mix que prepara a base de um projeto Elixir. Isto inclui o conteúdo do `mix.exs`, onde você define as especificidades do seu projeto, como dependências, configuração e tarefas personalizadas.

## Veja Também:
- Documentação oficial do `mix`: https://hexdocs.pm/mix/Mix.html
- Elixir Getting Started Guide: https://elixir-lang.org/getting-started/introduction.html
- Elixir School, uma coleção de lições sobre programação Elixir: https://elixirschool.com/
