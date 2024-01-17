---
title:                "Imprimindo saída de depuração"
html_title:           "Elixir: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## O que & Porquê?
Imprimir saída de depuração é uma técnica comum usada por programadores para ajudar a identificar e solucionar problemas em seus códigos. Ao incluir instruções de depuração em seu código, você pode ver passo a passo como os valores das variáveis estão mudando e onde o código pode estar falhando.

## Como fazer:
Para imprimir saída de depuração em Elixir, você pode simplesmente usar a função `IO.inspect`. Por exemplo:

```
Elixir IO.inspect("Olá, mundo!")
```

Isso irá imprimir a string "Olá, mundo!" no terminal.

Você também pode usar a função `inspect` para ver os valores das variáveis ​​em seu código. Por exemplo:

```
Elixir a = 5
Elixir IO.inspect(a)
```

Isso irá imprimir "5" no terminal, permitindo que você veja o valor atual de "a".

## Profundidade:
A impressão de saída de depuração existe há décadas e continua sendo um recurso fundamental para os programadores. Outras linguagens de programação também oferecem ferramentas de depuração mais avançadas, como pontos de interrupção e inspeção de variáveis em tempo real. No entanto, a impressão de saída de depuração ainda é amplamente usada devido à sua simplicidade e eficácia.

Além da função `IO.inspect`, também existem outras bibliotecas e ferramentas disponíveis em Elixir para impressão de saída de depuração, como a biblioteca `IEx.pry` e a ferramenta `Observer`.

## Veja também:
- [documentação oficial do Elixir sobre a função IO.inspect](https://hexdocs.pm/elixir/IO.html#inspect/2)
- [artigo sobre a importância da impressão de saída de depuração em programação](https://www.thoughtco.com/using-debug-output-in-programming-2033859)
- [repositório Github do IEx.pry](https://github.com/hauleth/pry)