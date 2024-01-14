---
title:    "Elixir: Iniciando um novo projeto"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por que usar Elixir para iniciar um novo projeto?

Elixir é uma linguagem de programação funcional, concorrente e escalável, que tem ganhado cada vez mais popularidade na comunidade de desenvolvedores. Com uma sintaxe clara e elegante, recursos poderosos, como concorrência e tolerância a falhas, e uma comunidade ativa e acolhedora, Elixir é uma ótima escolha para iniciar um novo projeto.

## Como começar um projeto em Elixir

Para começar um projeto em Elixir, siga os seguintes passos:

1. Instale o Elixir em seu computador.
2. Crie um novo diretório para o seu projeto.
3. Inicialize um projeto Elixir no diretório usando o comando `mix new nome-do-projeto`.
4. Navegue até o diretório do seu projeto e abra o arquivo `mix.exs` para adicionar dependências e configurar seu aplicativo.
5. Crie seus módulos e funções em arquivos `.ex` no diretório `lib` do seu projeto.
6. Para compilar e executar o seu projeto, use o comando `mix compile` e `mix run`.

Aqui está um exemplo de uma função simples em Elixir para adicionar dois números e retornar o resultado:

```elixir
defmodule Soma do
  def somar(a, b) do
    a + b
  end
end

Soma.somar(1, 2) # Saída: 3
```

## Mergulho profundo em iniciar um projeto Elixir

Iniciar um projeto em Elixir pode ser intimidante para aqueles que não estão familiarizados com a linguagem. No entanto, com a documentação abrangente do Elixir, incluindo o livro "Programming Elixir" de Dave Thomas, e a comunidade ativa no Discord e Reddit, você sempre terá recursos e suporte para ajudá-lo a começar.

Para tornar o processo de início ainda mais fácil, você também pode usar uma ferramenta chamada Phoenix para criar aplicativos web em Elixir de forma rápida e eficiente.

## Veja também

- [Elixir School](https://elixirschool.com/pt/)
- [Documentação do Elixir](https://elixir-lang.org/getting-started/introduction.html)
- [Phoenix Framework](https://www.phoenixframework.org/)