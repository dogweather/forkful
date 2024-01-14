---
title:                "Elixir: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que imprimir saída de depuração
Às vezes, ao trabalhar com Elixir, você pode se deparar com problemas ou bugs que são difíceis de detectar sem alguns dados extras. É aí que imprimir a saída de depuração pode ser útil. Ao adicionar instruções de exibição de dados ao seu código, você terá uma visão mais clara do que está acontecendo por trás das cenas.

## Como fazer
Para imprimir saída de depuração no Elixir, use a função `IO.inspect/2`. Esta função é built-in no Elixir e permite que você imprima qualquer valor dentro do seu código. Aqui está um exemplo de como usá-la:

```elixir
defmodule MyModule do
  def my_function(arg) do
    IO.inspect(arg)
    # rest of the code
  end
end
```

Neste exemplo, estamos usando `IO.inspect/2` com o argumento `arg`. Isso pode ser um valor, uma variável ou até mesmo uma expressão. Durante a execução do código, o valor de `arg` será impresso na saída. Aqui está um exemplo de saída:

```
"Hello, world!"
```

Observe que a saída é entre aspas, o que significa que `arg` é uma string. Você pode usar `IO.inspect/2` em qualquer lugar dentro do seu código para imprimir valores e acompanhar o fluxo e alterações de dados.

## Profundidade de mergulho
Além de simplesmente imprimir valores, `IO.inspect/2` também aceita opções adicionais. Aqui estão algumas que você pode achar úteis:

- `:label` - adiciona um rótulo ao valor impresso
- `:pretty` - formata a saída de forma mais legível
- `:limit` - limita o tamanho da saída impressa

Por exemplo, vamos usar `IO.inspect/2` com a opção `:label` em nosso exemplo anterior:

```elixir
defmodule MyModule do
  def my_function(arg) do
    IO.inspect(arg, label: "Argumento:")
    # rest of the code
  end
end
```

Isso resultará em uma saída como esta:

```
Argumento: "Hello, world!"
```

Além disso, você também pode imprimir várias coisas ao mesmo tempo, separando-as com uma vírgula. Vamos tentar:

```elixir
defmodule MyModule do
  def my_function(arg) do
    IO.inspect(arg, label: "Argumento:", pretty: true), IO.inspect(arg * 2, label: "Argumento multiplicado:", pretty: true)
    # rest of the code
  end
end
```

Isso nos dará a seguinte saída:

```
Argumento: "Hello, world!"
Argumento multiplicado: "Hello, world!Hello, world!"
```

Isso pode ser especialmente útil ao depurar código com vários valores ou em loops. Experimente e veja como ele pode ajudar a entender o que seu código está fazendo.

## Ver Além
Para mais informações sobre `IO.inspect/2`, confira a [documentação oficial do Elixir](https://hexdocs.pm/elixir/IO.html#inspect/2). Você também pode aprender mais sobre depuração em Elixir com [este guia](https://elixir-lang.org/getting-started/debugging.html) do site oficial do Elixir.