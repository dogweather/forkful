---
title:                "Lendo argumentos da linha de comando"
html_title:           "Haskell: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Ler argumentos da linha de comando é uma prática comum em programação, onde o programa recebe informações diretamente do usuário através da linha de comando. Isso permite que os programas sejam mais dinâmicos e interativos, podendo ser executados com diferentes opções e parâmetros.

## Como fazer:

Para ler argumentos da linha de comando em Haskell, podemos utilizar a função `getArgs` do módulo `System.Environment`. Esta função retorna uma lista de strings contendo os argumentos passados na linha de comando.

Exemplo de código:

```Haskell
module Main where

import System.Environment

main = do
  args <- getArgs
  putStrLn ("Argumentos recebidos: " ++ show args)
```

Exemplo de saída:

```
> programa arg1 arg2
Argumentos recebidos: ["arg1","arg2"]
```

## Aprofundando:

A leitura de argumentos da linha de comando é um recurso presente em diversas linguagens de programação, incluindo C, Python e Ruby. Em Haskell, a função `getArgs` é implementada de forma eficiente utilizando chamadas do sistema operacional.

Uma alternativa à leitura de argumentos da linha de comando é a utilização de flags, que são opções definidas pelo programador e passadas na linha de comando. Alguns módulos em Haskell, como o `System.Console.GetOpt`, facilitam a utilização de flags em programas.

## Veja também:

- [Documentação Haskell sobre a função getArgs](https://hackage.haskell.org/package/base-4.14.0.0/docs/System-Environment.html#g:7)
- [Exemplo de utilização de flags em Haskell](https://wiki.haskell.org/Flags)
- [Documentação Haskell sobre o módulo GetOpt](https://hackage.haskell.org/package/base-4.14.0.0/docs/System-Console-GetOpt.html)