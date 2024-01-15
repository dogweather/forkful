---
title:                "Iniciando um novo projeto"
html_title:           "Elixir: Iniciando um novo projeto"
simple_title:         "Iniciando um novo projeto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por que

Você decidiu começar um novo projeto em Elixir? Bem, há muitas razões pelas quais alguém pode querer começar um novo projeto em Elixir. Talvez seja porque você está procurando uma linguagem de programação funcional que seja fácil de aprender e com desempenho excelente. Talvez seja porque você quer aproveitar a escalabilidade e tolerância a falhas que Elixir oferece. Ou talvez seja porque você quer aprender uma nova linguagem de programação para expandir suas habilidades. Seja qual for a sua razão, vamos ver como começar um novo projeto em Elixir.

## Como começar um novo projeto em Elixir

Para começar um novo projeto em Elixir, você vai precisar ter o Elixir instalado no seu computador. Depois de instalado, você pode usar o comando `mix new` para criar um novo projeto. Por exemplo:

```elixir
mix new meu_projeto
```

Isso irá criar uma estrutura básica para o seu projeto, incluindo alguns arquivos e pastas iniciais. Agora você pode adicionar o código do seu projeto nos arquivos criados. Por exemplo, vamos adicionar uma função que imprime "Olá mundo!" no console:

```elixir
defmodule MeuProjeto do
  def hello do
    IO.puts("Olá mundo!")
  end
end

MeuProjeto.hello
```

Para executar o código acima, use o comando `mix run` seguido do nome do arquivo. No nosso caso, seria:

```elixir
mix run lib/meu_projeto.exs
```

Isso irá imprimir "Olá mundo!" no console. Agora você pode explorar mais recursos de Elixir e construir seu novo projeto.

## Mais sobre começar um novo projeto

Ao começar um novo projeto em Elixir, é importante se familiarizar com a estrutura padrão do projeto e os arquivos gerados. Além disso, é recomendado utilizar ferramentas de gerenciamento de dependências, como o Hex, para instalar e gerenciar pacotes externos. E, é claro, não deixe de conferir a documentação oficial de Elixir para obter mais informações.

## Veja também

- [Elixir - Página oficial](https://elixir-lang.org/)
- [Hex - Gerenciador de dependências para Elixir](https://hex.pm/)
- [Documentação de Elixir](https://hexdocs.pm/elixir/)