---
title:                "Haskell: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto em Haskell?

Escrever um arquivo de texto é uma tarefa comum em programação e pode ser especialmente útil em Haskell. Pode ser uma boa ideia escrever um arquivo de texto para armazenar dados ou resultados de um programa.

## Como fazer

Para escrever um arquivo de texto em Haskell, primeiro é preciso importar o módulo `System.IO`. Em seguida, é necessário abrir um arquivo usando a função `openFile`. Veja um exemplo abaixo:

```Haskell
import System.IO

main = do
  file <- openFile "meu_arquivo.txt" WriteMode
  hPutStrLn file "Este é um exemplo de texto que será escrito no arquivo."
  hClose file
```

No exemplo acima, `meu_arquivo.txt` será criado ou, se já existir, será sobrescrito. A função `hPutStrLn` escreve uma linha de texto no arquivo, seguida pela a função `hClose` para fechar o arquivo.

## Mergulho Profundo

Escrever um arquivo de texto também pode ser feito utilizando a função `withFile`, que gerencia automaticamente a abertura e fechamento do arquivo. Além disso, é possível utilizar a notação `writeFile` para escrever todo o conteúdo de uma vez, ao invés de linha por linha.

```Haskell
import System.IO

main = do
  withFile "meu_arquivo.txt" WriteMode (\file -> do
    hPutStrLn file "Este é um exemplo de texto que será escrito no arquivo."
    hPutStrLn file "Esta é outra linha escrita utilizando a notação writeFile."
    )
```

## Veja também

- [Guia de Estilo de Código Haskell](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md)
- [Tutorial de Programação Haskell](https://www.haskell.org/tutorial/index.html)
- [Documentação do Módulo System.IO](https://www.haskell.org/platform/doc/2014.2.0.0/ghc-doc/libraries/base-4.7.0.2/System-IO.html)