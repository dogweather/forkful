---
title:                "Começando um novo projeto"
html_title:           "Elixir: Começando um novo projeto"
simple_title:         "Começando um novo projeto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

# O que e por que?

Começar um novo projeto é o processo de iniciar um novo projeto de programação, onde os programadores desenvolvem novos programas ou aplicativos. Isso é feito para solucionar problemas ou atender às necessidades específicas dos usuários. Novos projetos também são criados para melhorar ou substituir programas antigos.

## Como fazer:

```
Elixir.new/1
```

Este é o comando usado para criar um novo projeto em Elixir. Ele irá criar uma estrutura básica de diretórios e arquivos para o seu novo projeto. Você pode especificar o nome do projeto como argumento. 

```
Elixir.new("meu_projeto")
```

A saída será algo como:

```
Generating project meu_projeto
* creating README.md
* creating .formatter.exs
* creating .gitignore
* creating mix.exs
* creating config
* creating config/config.exs
* creating lib
* creating lib/meu_projeto.ex
* creating test
* creating test/test_helper.exs
* creating test/meu_projeto_test.exs

```

## Exploração aprofundada:

Elixir é uma linguagem de programação funcional, criada em 2011 por José Valim. Ele combina conceitos de outras linguagens, como Erlang e Ruby, para fornecer um ambiente altamente escalável e produtivo.

Se você não quiser usar o comando `Elixir.new/1` para começar um novo projeto, também pode usar ferramentas como o Phoenix Framework ou o Mix, que oferecem recursos adicionais para criar e gerenciar projetos em Elixir.

Ao iniciar um novo projeto em Elixir, é importante considerar as estruturas de dados e as funções que serão utilizadas. Elixir possui uma forte ênfase na concorrência e na imutabilidade, o que pode ser útil ao criar aplicativos escaláveis e robustos.

## Veja também:

- [Phoenix Framework](https://www.phoenixframework.org/)
- [Mix](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html)
- [Documentação oficial do Elixir](https://elixir-lang.org/docs.html)