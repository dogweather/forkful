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

## O que e por que?

Verificar se um diretório existe é uma tarefa comum na programação. Isso permite que os programadores verifiquem se um determinado diretório está presente no sistema de arquivos antes de executar ações que dependem dele. Isso pode ajudar a evitar erros e garantir que o código funcione corretamente.

## Como fazer:

Para verificar se um diretório existe em Haskell, podemos usar a função `doesDirectoryExist` do módulo `System.Directory`. Essa função recebe um caminho de diretório como argumento e retorna um valor booleano indicando se o diretório existe ou não.

Exemplo de código:

```Haskell
import System.Directory

main = do
    let dir = "/home/usuario/documentos"
    exist <- doesDirectoryExist dir
    if exist
        then putStrLn "O diretório existe!"
        else putStrLn "O diretório não existe."
```

Saída:

```
O diretório existe!
```

## Profundidade:

Verificar a existência de um diretório é uma tarefa que pode parecer trivial, mas é importante para garantir a estabilidade e funcionalidade do código. Antes do lançamento do Haskell 1.4 em 1997, não havia uma função embutida para verificar a existência de diretórios. Em vez disso, os programadores precisavam usar funções do sistema operacional, o que tornava o código menos portátil e mais propenso a erros.

Além da função `doesDirectoryExist`, o módulo `System.Directory` também possui outras funções para gerenciar diretórios, como `createDirectory` e `removeDirectory`. No entanto, existem outras bibliotecas disponíveis que oferecem funcionalidades semelhantes, como o pacote `directory-tree` e o módulo `System.Posix.Directory`, que é específico para sistemas POSIX.

No nível de implementação, a função `doesDirectoryExist` usa a função `getFileStatus` internamente para obter informações sobre o caminho do diretório. Em seguida, é verificado se o tipo de arquivo é um diretório e o resultado é retornado como um valor booleano.

## Veja também:

- [Módulo `System.Directory`](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Pacote `directory-tree`](https://hackage.haskell.org/package/directory-tree)
- [Módulo `System.Posix.Directory`](https://hackage.haskell.org/package/unix/docs/System-Posix-Directory.html)