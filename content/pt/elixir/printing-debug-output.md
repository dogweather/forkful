---
title:    "Elixir: Imprimindo saída de depuração"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Por que

Debugging é uma parte importante do processo de desenvolvimento de software. É através do processo de depuração que podemos encontrar e resolver erros em nosso código. No entanto, pode ser difícil rastrear e entender o comportamento de um programa sem a ajuda de ferramentas de debug. A impressão de saída de debug é uma técnica útil para visualizar os valores de variáveis e o fluxo de execução do código, o que pode facilitar muito o processo de localizar e corrigir erros.

## Como fazer

Para imprimir saída de debug em Elixir, podemos usar a função `IO.inspect/2`. Esta função aceita dois argumentos: o valor que queremos inspecionar e uma opção para formatar a saída. Veja um exemplo de uso da função `IO.inspect/2` abaixo:

```Elixir
defmodule Exemplo do
  x = "Olá!"
  IO.inspect(x, label: "Valor de x:")
end
```

A saída deste código seria:

```
Valor de x: "Olá!"
```

Podemos usar a opção `label` para especificar um rótulo para a saída, o que pode ajudar a identificar o que está sendo inspecionado. Além disso, podemos usar as opções `:pretty` e `:width` para formatar a saída de forma mais legível.

## Profundidade no assunto

Ao usar a função `IO.inspect/2`, podemos imprimir não apenas valores de variáveis, mas também expressões e até mesmo funções. Podemos até mesmo inspecionar o valor de retorno de uma função sem precisar atribuí-lo a uma variável. Por exemplo:

```Elixir
IO.inspect(div(10, 2))
```

A saída deste código seria:

```
5
```

Além disso, podemos usar a função `IO.inspect/2` dentro de outras funções, permitindo que visualizemos o estado de variáveis em diferentes pontos do código.

## Veja também

- [Documentação oficial da função `IO.inspect/2`](https://hexdocs.pm/elixir/IO.html#inspect/2)
- [Artigo sobre debug em Elixir](https://medium.com/@jeremyjh/elixir-debugging-techniques-8116ed92d0d9)
- [Vídeo explicando como usar a função `IO.inspect/2`](https://www.youtube.com/watch?v=wVHbHbMjDgs)