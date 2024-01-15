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

## Por que

Já se perguntou como pode interagir com seu programa Elixir através da linha de comando? Ler argumentos de linha de comando pode ser muito útil para personalizar a execução do seu programa e torná-lo mais flexível. Neste artigo, vamos explorar como fazer isso usando o Elixir!

## Como Fazer

Para ler argumentos de linha de comando em Elixir, podemos usar o módulo `System` e sua função `argv`. Esta função retorna uma lista contendo todos os argumentos passados ​​na linha de comando.

Vamos ver isso em ação com um exemplo simples:

```Elixir
# programa.exs
args = System.argv
IO.inspect(args)
```

Ao executar este programa no terminal, podemos passar os argumentos após o comando `elixir programa.exs`, como por exemplo:

```terminal
elixir programa.exs arg1 arg2
```

A saída será:

```terminal
["arg1", "arg2"]
```

Podemos também usar padrões de correspondência para tratar argumentos específicos. Por exemplo, se quisermos apenas imprimir o segundo argumento, podemos fazer o seguinte:

```Elixir
# programa.exs
[_ | argumento] = System.argv
IO.puts("O segundo argumento é: #{argumento}")
```

Agora, ao executar o programa como `elixir programa.exs arg1 arg2`, a saída será:

```terminal
O segundo argumento é: arg2
```

## Deep Dive

O módulo `System` oferece outras funções úteis para trabalhar com argumentos de linha de comando, como `get_env`, que obtém o valor de uma variável de ambiente passada como argumento na linha de comando. Além disso, também podemos usar `arg` para obter um argumento específico pelo seu índice.

É importante lembrar que quando usamos `System.argv`, a lista retornada incluirá o nome do próprio programa como primeiro elemento. Portanto, podemos usar `hd` para obter somente os argumentos passados a partir do segundo elemento em diante.

Para mais informações sobre como trabalhar com argumentos de linha de comando em Elixir, consulte a [documentação oficial](https://hexdocs.pm/elixir/System.html#argv/0) e o [artigo em inglês](https://blog.appsignal.com/2020/01/15/command-line-arguments-in-elixir.html).

## Veja Também

- [Documentação Oficial do Elixir sobre `System.argv`](https://hexdocs.pm/elixir/System.html#argv/0)
- [Artigo em inglês sobre argumentos de linha de comando em Elixir](https://blog.appsignal.com/2020/01/15/command-line-arguments-in-elixir.html)