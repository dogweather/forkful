---
title:                "Usando um shell interativo (REPL)"
date:                  2024-01-26T04:13:09.389595-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando um shell interativo (REPL)"

category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## O Que e Por Que?
Um shell interativo, ou REPL (Leia-Avalie-Imprima Loop), permite experimentar trechos de código em tempo real. Programadores Elixir utilizam o REPL, chamado IEx (Elixir Interativo), para experimentar, depurar, e aprender a linguagem.

## Como:
Para iniciar o IEx, abra seu terminal e digite `iex`. Aqui vai uma amostra:

```Elixir
iex> name = "Programador Elixir"
"Programador Elixir"
iex> String.length(name)
17
iex> Enum.map([1, 2, 3], fn num -> num * 3 end)
[3, 6, 9]
```

A saída deve mostrar a atribuição de variável, os resultados de função e uma função anônima em ação.

## Mergulho Profundo
O shell IEx faz parte do Elixir desde seus primeiros dias. José Valim, o criador do Elixir, se inspirou nos shells interativos de outras linguagens como o `python` do Python e o `irb` do Ruby. Embora o IEx compartilhe muitas características com estes, ele é construído para lidar com a natureza concorrente do Elixir e é totalmente integrado com as capacidades da VM Erlang.

Alternativas ao IEx no ecossistema Erlang incluem `erl`, o shell Erlang. Mas o IEx fornece um ambiente mais amigável ao Elixir, com recursos como auto completar abrangente, histórico e ajudantes.

O REPL IEx é mais que um playground; ele pode se conectar de forma transparente a um sistema em execução. Isso é crucial para depurar aplicações ao vivo. A implementação subjacente depende do BEAM (a VM Erlang), garantindo que recursos como a troca de código quente sejam suportados diretamente no shell.

## Veja Também
Confira estes recursos para leitura e informações adicionais:

- [Documentação do IEx do Elixir](https://hexdocs.pm/iex/IEx.html)
- [Elixir Interativo (IEx) - O Shell do Elixir](https://elixir-lang.org/getting-started/introduction.html#interactive-elixir)
- [Documentação do `erl` do Erlang](http://erlang.org/doc/man/erl.html)
- [Aprendendo o Shell Interativo do Elixir](https://elixirschool.com/en/lessons/basics/iex_helpers/)
