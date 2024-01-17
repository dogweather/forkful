---
title:                "Escrevendo no erro padrão"
html_title:           "Haskell: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O que & Porquê?

Escrever para o erro padrão (standard error) é uma prática comum entre programadores para relatar possíveis erros ou problemas durante a execução do código. Ao enviar mensagens de erro para o erro padrão, é possível identificar e corrigir falhas no código de maneira mais eficiente.

## Como fazer:

Existem várias maneiras de escrever para o erro padrão em Haskell. Aqui estão dois exemplos simples utilizando o comando "putStrLn" e a função "hPutStrLn":

```Haskell
import System.IO

putStrLn "Mensagem de erro"
hPutStrLn stderr "Outra mensagem de erro"
```
Saída:
```
Mensagem de erro
Outra mensagem de erro
```

## Mergulho Profundo:

A prática de escrever para o erro padrão tem suas raízes na programação de sistemas operacionais Unix, onde havia diferentes canais de saída para relatar erros e mensagens de sistema. Além do uso do comando "putStrLn" e da função "hPutStrLn", existem outras funções disponíveis para escrever no erro padrão, como "hPutStr" e "hPutChar". Também é possível redirecionar o erro padrão para um arquivo ou outro dispositivo de saída.

## Veja Também:

- [Documentação do Haskell](https://www.haskell.org/documentation/)
- [Artigo sobre o comando putStrLn](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:putStrLn)
- [Artigo sobre a função hPutStrLn](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html#v:hPutStrLn)