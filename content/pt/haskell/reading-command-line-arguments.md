---
title:                "Lendo argumentos de linha de comando."
html_title:           "Haskell: Lendo argumentos de linha de comando."
simple_title:         "Lendo argumentos de linha de comando."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando?

Ler argumentos da linha de comando é uma habilidade importante para programadores em Haskell, especialmente quando se trata de criar programas com interfaces de usuário ou scripts que precisam de entrada dinâmica. Saber como ler corretamente os argumentos da linha de comando pode tornar suas aplicações mais interativas e fáceis de usar.

## Como fazer?

Ler argumentos da linha de comando em Haskell é uma tarefa simples e direta. Basta seguir alguns passos:

1. Importe o módulo `System.Environment` para ter acesso às funções que lidam com argumentos da linha de comando.
2. Use a função `getArgs` para retornar uma lista de todos os argumentos passados na linha de comando.
3. Converta a lista de `String` para o tipo desejado, caso necessário.

Aqui está um exemplo de código mostrando como ler e imprimir os argumentos da linha de comando:

```Haskell
import System.Environment

main = do
    args <- getArgs
    putStrLn ("Os argumentos são: " ++ show args)
```

Ao executar este programa com os argumentos `hello world`, a saída será:

```
Os argumentos são: ["hello", "world"]
```

## Aprofundando

Existem outras formas de ler argumentos da linha de comando em Haskell, como usar a função `getProgName` para obter o nome do programa em si, ou a função `lookupEnv` para verificar se determinada variável de ambiente foi definida. Além disso, é possível lidar com argumentos de linha de comando opcionais usando a biblioteca `optparse-applicative`.

## Veja também

Aqui estão alguns links úteis para aprender mais sobre como ler argumentos da linha de comando em Haskell:

- [Documentação oficial do módulo System.Environment](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-Environment.html)
- [Tutorial sobre argumentos de linha de comando em Haskell](https://www.corylogan.com/programming/2012/07/15/getting-command-line-arguments-in-haskell/)
- [Tutorial sobre a biblioteca optparse-applicative](https://github.com/pcapriotti/optparse-applicative#optparse-applicative)