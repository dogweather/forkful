---
title:                "Verificando se um diretório existe"
html_title:           "Haskell: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que

Se você está trabalhando em um projeto de programação que precisa acessar ou manipular arquivos dentro do sistema operacional, é importante verificar se um diretório existe antes de prosseguir. Isso garante que seu código não sofra erros e que o programa seja executado corretamente.

## Como fazer

A linguagem de programação Haskell oferece uma função para verificar se um diretório existe dentro do sistema operacional. Veja um exemplo de como usar esta função:

```Haskell
import System.Directory

main = do
    exists <- doesDirectoryExist "caminho/do/diretorio"
    print exists
```

Neste exemplo, importamos o módulo System.Directory, que contém a função doesDirectoryExist. Em seguida, definimos uma função principal (main) utilizando a sintaxe do Haskell, que irá verificar se o diretório especificado existe e imprimir o resultado (True ou False) na tela.

## Deep Dive

Ao utilizar a função doesDirectoryExist, é importante entender como ela funciona e quais parâmetros podem ser adicionados para tornar a verificação mais precisa. A função possui a seguinte assinatura:

```Haskell
doesDirectoryExist :: FilePath -> IO Bool
```

Ela recebe como parâmetro o caminho do diretório a ser verificado (FilePath) e retorna um valor do tipo IO Bool, que pode ser True (se o diretório existe) ou False (se o diretório não existe).

Além disso, a função também é capaz de verificar se o caminho fornecido é um diretório válido, ou seja, se ele está acessível pelo sistema operacional. Portanto, é importante garantir que o caminho informado esteja correto antes de utilizar a função.

## Veja também

- Documentação oficial da função doesDirectoryExist: https://hackage.haskell.org/package/directory/docs/System-Directory.html#v:doesDirectoryExist
- Tutorial básico sobre Haskell: https://www.haskell.org/tutorial/