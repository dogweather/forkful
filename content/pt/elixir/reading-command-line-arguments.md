---
title:                "Lendo argumentos da linha de comando"
html_title:           "Elixir: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O que e porquê?

Ler argumentos de linha de comando é uma técnica importante para qualquer programador. Ela permite que o seu programa receba informações diretamente do usuário através da linha de comando, tornando-o mais interativo e flexível. É especialmente útil para programas de linha de comando que precisam ser executados rapidamente e com diferentes opções.

## Como fazer:

Utilizando Elixir, você pode ler argumentos de linha de comando facilmente através da função `System.argv/0`. Ela retorna uma lista contendo todos os argumentos passados na linha de comando. Veja um exemplo simples abaixo:

```elixir
# Lê argumentos de linha de comando
args = System.argv()

# Imprime a lista de argumentos
IO.inspect(args)
```

Ao executar o programa acima com os parâmetros `elixir meu_programa.ex argumento1 argumento2`, a saída será:

```
["argumento1", "argumento2"]
```

## Detalhes aprofundados:

A leitura de argumentos de linha de comando não é uma técnica nova e é amplamente utilizada em diferentes linguagens de programação. Em Elixir, as funções `System.argv/0` e `System.argv/1` (que retorna o nome do programa) são as principais ferramentas para essa tarefa. No entanto, também é possível usar a biblioteca OptionParser para criar um parser de argumentos mais sofisticado.

## Veja também:

- Documentação oficial do `System.argv`: https://hexdocs.pm/elixir/System.html#argv/0
- Documentação oficial do OptionParser: https://hexdocs.pm/elixir/OptionParser.html