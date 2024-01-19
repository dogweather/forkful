---
title:                "Lendo argumentos de linha de comando"
html_title:           "Arduino: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
A leitura de argumentos da linha de comando refere-se à aptidão de um programa em processar informações inseridas através do terminal do sistema operacional. É útil para personalizar a execução de um script, como ajustar configurações ou especificar a fonte de um arquivo de dados.

## Como fazer:
Para ler argumentos da linha de comando em Elixir, você usa a função padrão `System.argv()`. Este é um exemplo que lista cada argumento:

```Elixir
Enum.each(System.argv(), fn arg -> IO.puts(arg) end)
```
Se você executar `elixir script.exs arg1 arg2 arg3`, a saída será:

```shell
arg1
arg2
arg3
```

## Mergulho Profundo
A capacidade de ler argumentos da linha de comando é uma prática comum na programação devido à sua origem nos sistemas operacionais baseados em Unix. Esta funcionalidade oferece uma interface conveniente para manipular a execução de um programa.

Há alternativas dependendo do seu caso de uso. Se você precisa de uma análise mais complexa de argumentos, como suporte a bandeiras ("/f"), o Elixir tem a opção de utilizar bibliotecas externas como a `OptionParser`.

Os argumentos de linha de comando em Elixir são implementados como uma lista de strings. Cada item na lista representa um argumento adicional após o nome do script. Esta lista é retornada ao chamar `System.argv()`.

## Ver Também
1. Documentação oficial da System.argv(): https://hexdocs.pm/elixir/System.html#argv/0
2. Guia para OptionParser: https://hexdocs.pm/elixir/OptionParser.html
3. Artigo sobre o uso de argumentos da linha de comando: https://thinkingelixir.com/understanding-command-line-arguments-in-elixir-1-3-scripts/