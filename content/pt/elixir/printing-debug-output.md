---
title:                "Imprimindo saída de debug"
html_title:           "C#: Imprimindo saída de debug"
simple_title:         "Imprimindo saída de debug"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Imprimir debug output é o processo de usar código para mostrar informações sobre o estado atual de um programa. Os programadores fazem isso para ajudar a rastrear a operação de um programa e identificar problemas - é uma parte valiosa da solução de problemas.

## Como fazer:
Vamos falar sobre Elixir (versão mais recente), um idioma funcional que facilita a impressão da saída de debug.

Para imprimir debug output em um script Elixir, usamos as funções IO.puts, IO.inspect.

```Elixir
# Para exibir uma mensagem simples:
IO.puts("Isso é uma mensagem de saída de debug.")

# Para exibir uma variável:
nome = "Pedro"
IO.puts("O nome do usuário é #{nome}")

# Para examinar o conteúdo de uma lista:
lista = [1, 2, 3]
IO.inspect(lista)
```

Quando você executa este exemplo, a saída será:

```
Isso é uma mensagem de saída de debug.
O nome do usuário é Pedro
[1, 2, 3]
```

## Mergulho Profundo
O IO.inspect é uma ferramenta poderosa no Elixir para depuração. Ele derivou da linguagem de programação Erlang, e ao contrário do IO.puts, que só pode lidar com strings, o IO.inspect pode lidar com qualquer tipo de dados, tornando-o uma escolha superior.

Como alternativa à escrita de código de saída de debug no seu código, você pode usar o depurador interno do Elixir, que permite que você execute seu código passo a passo.

Além disso, um ponto importante é que o IO.inspect retorna o valor inspecionado, tornando-o ainda mais fácil de colocar em qualquer lugar em seu código sem alterar o resultado de sua função - tornando-o ideal para a verificação rápida de valores.

## Veja Também
Para mais informação confira:

- Elixir School: [Elixir School - Basics (Debugging)](https://elixirschool.com/pt/lessons/basics/debugging/)
- Documentação Oficial do Elixir: [IO.inspect](https://hexdocs.pm/elixir/IO.html#inspect/2)
- Depurador Elixir: [Debugger - Debugging](https://elixir-lang.org/getting-started/debugging.html)