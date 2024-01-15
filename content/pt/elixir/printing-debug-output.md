---
title:                "Saida de depuração de impressão"
html_title:           "Elixir: Saida de depuração de impressão"
simple_title:         "Saida de depuração de impressão"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que

Às vezes, ao escrever código em Elixir, pode ser necessário entender o que está acontecendo em cada etapa do programa. Nesses casos, imprimir mensagens de debug pode ser muito útil para entender o fluxo do programa e detectar possíveis erros.

## Como fazer

Para imprimir uma mensagem de debug em Elixir, podemos utilizar a função `IO.inspect/1` seguida do valor que desejamos imprimir. Por exemplo:

```Elixir
IO.inspect("Olá, mundo!")
```

Isso irá imprimir a mensagem "Olá, mundo!" no terminal ao executar o programa. Também é possível incluir variáveis e até mesmo expressões dentro da função `IO.inspect/1` para obter mais informações sobre o estado atual do programa.

```Elixir
nome = "Maria"
idade = 30
IO.inspect("O nome é #{nome} e a idade é #{idade}")
```

Isso irá imprimir a mensagem "O nome é Maria e a idade é 30". Além disso, também é possível utilizar a função `IO.inspect/2` para imprimir mensagens coloridas e formatadas.

## Aprofundando

Imprimir mensagens de debug pode ser uma forma simples e eficaz de entender o que está acontecendo em um programa complexo. Além disso, a função `IO.inspect/1` também é muito útil para verificar valores de variáveis em diferentes partes do código e detectar possíveis problemas.

Uma dica importante é remover todas as chamadas de `IO.inspect/1` após terminar de depurar o código, pois elas podem afetar negativamente o desempenho do programa em produção. No entanto, se você ainda quiser manter algumas chamadas de debug para uso futuro, é possível envolvê-las em uma diretiva `if` que verifique se o ambiente é de desenvolvimento ou teste.

## Veja também

- <https://elixir-lang.org/getting-started/debugging.html>
- <https://erlang.org/doc/apps/erts/io_protocol.html>
- <https://hexdocs.pm/iex/IEx.Helpers.html>