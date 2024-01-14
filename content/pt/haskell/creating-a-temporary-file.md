---
title:    "Haskell: Criando um arquivo temporário"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Porque criar um arquivo temporário em Haskell

Criar arquivos temporários é uma prática comum na programação para armazenar dados temporários ou para fins de teste. Isso pode ser especialmente útil ao lidar com arquivos grandes que não podem ser armazenados na memória principal ou quando precisamos testar um código antes de implementá-lo definitivamente. Neste artigo, vamos explorar como criar e manipular arquivos temporários em Haskell.

## Como criar um arquivo temporário em Haskell

Para criar um arquivo temporário em Haskell, usamos a função `withSystemTempFile` do módulo `System.IO.Temp`. Esta função recebe dois parâmetros: o padrão de nome do arquivo e uma função que manipula esse arquivo. A função passada deve ter dois parâmetros: o caminho para o arquivo temporário e o handle (identificador) do arquivo.

Um exemplo de código para criar e escrever em um arquivo temporário seria o seguinte:

```Haskell
import System.IO
import System.IO.Temp

main = withSystemTempFile "temp.txt" $ \tmpFilePath handle -> do
  hPutStrLn handle "Hello World!"
  hClose handle
```

Neste exemplo, estamos criando um arquivo temporário com o nome "temp.txt" e escrevendo "Hello World!" nele. A função `withSystemTempFile` se encarregará de criar o arquivo e gerenciar seu fechamento após a execução da função passada.

## Aprofundando no uso de arquivos temporários em Haskell

As operações de leitura e escrita em arquivos temporários podem ser realizadas usando as funções `hGetContents` e `hPutStr` do módulo `System.IO`. Além disso, podemos usar as funções `withSystemTempDirectory` e `withTempDirectory` para criar diretórios temporários em vez de arquivos.

Também é importante lembrar de remover os arquivos temporários após seu uso para não ocupar espaço desnecessário no sistema. Para isso, podemos usar a função `removePathForcibly` do módulo `System.Directory`.

## Veja também

- [Documentação oficial do módulo System.IO.Temp](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO-Temp.html)
- [Tutorial sobre manipulação de arquivos em Haskell](https://github.com/Mikail-Eli/Manipulando-Arquivos-em-Haskell)