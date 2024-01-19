---
title:                "Iniciando um novo projeto"
html_title:           "Javascript: Iniciando um novo projeto"
simple_title:         "Iniciando um novo projeto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Iniciar um novo projeto em programação significa criar uma base para o desenvolvimento de uma nova aplicação ou software. Programadores fazem isso para resolver problemas específicos, implementar novas funcionalidades, ou usar tecnologias emergentes, como Elixir.

## Como Fazer:

Elixir torna a criação de um novo projeto bastante simples. Usa-se o comando mix para gerar uma estrutura de diretório de projeto base. Por exemplo:

```Elixir
# create a new project
mix new my_project
```

Isso irá gerar um diretório chamado `my_project` com a seguinte estrutura:

```Elixir
/my_project
  /lib
    /my_project.ex
  /test
    /my_project_test.exs
  mix.exs
```

O arquivo `mix.exs` define o projeto e suas dependências.

## Mergulho Profundo:

- **Contexto Histórico**: Elixir é uma linguagem de programação funcional, concorrente, geralmente usada para construir aplicações escaláveis e manuteníveis. Lançada em 2012, é construída em cima da máquina virtual Erlang, permitindo aproveitar todos os benefícios do Erlang.

- **Alternativas**: Além do Elixir, linguagens como Python, Java ou JavaScript podem ser usadas para iniciar um novo projeto. No entanto, a escolha entre estas linguagens depende do objetivo do projeto e das competências do programador.

- **Implementação**: Ao iniciar um projeto em Elixir, a estrutura padrão inclui duas pastas principais: `lib`, onde o Elixir procura por código de compilação, e `test`, que deve conter os testes do projeto. O arquivo `mix.exs` é o coração do projeto, onde definimos o nome do projeto, a versão do Elixir, as dependências, entre outros.

## Ver Também:

- [Documentação Oficial Elixir](https://elixir-lang.org/docs.html)
- [Guia Getting Started em Elixir](https://elixir-lang.org/getting-started/introduction.html)
- [Erro Elixir como ferramenta para startups](https://blog.lelonek.me/elixir-as-a-tool-for-startups-6f9e56d806b3) 

Lembrando: Só a prática leva à perfeição, então comece a codar agora!