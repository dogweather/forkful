---
title:                "Haskell: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão

Escrever para o erro padrão é uma prática comum em linguagens de programação, incluindo Haskell. Isso permite que você monitore e registre possíveis erros ou informações importantes durante a execução do seu código.

## Como escrever para o erro padrão em Haskell

Para escrever para o erro padrão em Haskell, você pode utilizar a função `hPutStrLn` do módulo `System.IO`. Por exemplo, se você quiser imprimir uma mensagem de erro, você pode fazer o seguinte:

```Haskell
import System.IO

hPutStrLn stderr "Erro: Valor inválido."
```
Isso imprimirá a mensagem "Erro: Valor inválido." no seu terminal.

Além disso, você também pode capturar os erros gerados pelo seu código e escrevê-los para o erro padrão usando a função `hPrint`. Veja um exemplo:

```Haskell
import System.IO

divide :: Double -> Double -> IO ()
divide x y = do
    if y == 0
        then hPutStrLn stderr "Erro: Divisão por zero."
        else hPrint stderr $ x / y

main = do
    divide 10 2 -- Imprime "5.0"
    divide 10 0 -- Imprime "Erro: Divisão por zero."
```

## Aprofundando-se na escrita para o erro padrão

Além das funções `hPutStrLn` e `hPrint`, existem outras maneiras de escrever para o erro padrão em Haskell, como o uso de `hPutStr` e `hPutChar`. Além disso, é possível customizar a saída do erro padrão alterando o buffer com a função `hSetBuffering`. Vale a pena explorar as diferentes opções e escolher aquela que melhor se adequa às suas necessidades.

## Veja também

- [Documentação da função hPutStrLn](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html#v:hPutStrLn)
- [Tutorial sobre a manipulação de erros em Haskell](https://www.ahri.net/2011/04/14/error-handling-in-haskell/)
- [Artigo sobre como personalizar a saída do erro padrão em Haskell](https://ieeexplore.ieee.org/document/6979447)