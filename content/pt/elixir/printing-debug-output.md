---
title:                "Elixir: Imprimindo saída de depuração"
programming_language: "Elixir"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que Imprimir Saída de Debug

Às vezes, quando estamos codificando em Elixir, podemos nos deparar com situações em que algo não está funcionando da maneira que esperamos. Nesses momentos, precisamos de uma maneira rápida e fácil de descobrir o que está acontecendo nos bastidores do nosso código. É aí que a impressão de saída de debug pode ser útil. Ela nos permite visualizar valores de variáveis e outras informações importantes durante a execução do programa, o que pode nos ajudar a identificar e corrigir erros.

## Como Fazer

Para imprimir saída de debug em Elixir, podemos usar a função `IO.inspect/2`. Ela possui dois argumentos: o primeiro é o valor que queremos inspecionar, e o segundo é uma lista de opções. Vamos dar uma olhada em um exemplo:

```Elixir
iex> x = 5
5
iex> IO.inspect(x, label: "Valor de x:")
Valor de x: 5
```

Neste exemplo, definimos uma variável `x` com o valor `5` e usamos a função `IO.inspect` para imprimi-lo com um rótulo descrevendo o que é. Também podemos usar a função dentro de uma cadeia de chamadas de funções, como no exemplo a seguir:

```Elixir
iex> lista = [1, 2, 3]
[1, 2, 3]
iex> lista
|> Enum.map(&(&1 * 2))
|> IO.inspect(label: "Lista multiplicada por 2:")
Lista multiplicada por 2: [2, 4, 6]
```

Neste caso, usamos `IO.inspect` para imprimir a lista depois de aplicar a função `Enum.map` para multiplicar cada elemento por 2.

Além disso, podemos usar a função `IO.inspect/3` para especificar um nível de profundidade para inspecionar valores mais complexos. Por exemplo:

```Elixir
iex> mapa = %{a: 1, b: %{c: 2, d: 3}}
%{a: 1, b: %{c: 2, d: 3}}
iex> IO.inspect(mapa, label: "Mapa:", depth: 2)
Mapa: %{a: 1, b: %{}}
```

Neste caso, definimos um mapa com valores aninhados e usamos a função `IO.inspect` com um nível de profundidade de 2 para limitar a inspeção ao primeiro nível do mapa.

## Deep Dive

Além da função `IO.inspect`, existem outras maneiras de imprimir saída de debug em Elixir. Uma delas é usando o módulo `Logger`, que nos permite gravar mensagens de log em um arquivo ou saída do console. Por exemplo:

```Elixir
iex> Logger.debug("Mensagem de Debug")
:ok
```

Essa mensagem será registrada no arquivo de log `erlang.log` no diretório atual ou na saída do console, se nenhum arquivo de log for configurado.

Além disso, também podemos usar a macro `require Logger` para importar as funções do módulo `Logger` e usá-las diretamente, sem precisar escrever `Logger.debug` toda vez.

## Veja Também

* [Documentação do Elixir sobre Debugging](https://hexdocs.pm/elixir/debugging.html)
* [Erlang Logger](http://erlang.org/doc/man/logger.html)
* [Elixir Logger](https://hexdocs.pm/elixir/Logger.html)