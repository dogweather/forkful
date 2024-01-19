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

## O quê e por quê?

Verificar se um diretório existe é uma operação comum em programação. Trata-se de conferir se um caminho de diretório corresponde a um diretório existente. Fazemos isso para evitar erros ao tentar acessar ou manipular diretórios inexistentes.

## Como fazer:

Você pode verificar se um diretório existe em Haskell usando a função `doesDirectoryExist` do módulo `System.Directory`. Veja o exemplo a seguir:

```Haskell
import System.Directory

main = do
   b <- doesDirectoryExist "/caminho/para/o/diretorio"
   if b then putStrLn "O diretório existe"
     else putStrLn "O diretório não existe"
```

Quando executado, esse programa irá imprimir "O diretório existe" se o diretório '/caminho/para/o/diretorio' existir, e "O diretório não existe" no caso contrário.

## Aprofundando-se no assunto

O módulo `System.Directory` do Haskell fornece funções de manipulação de diretórios e arquivos. A função `doesDirectoryExist` foi introduzida na versão 1.2 do pacote `directory`, disponibilizado pela primeira vez com o GHC 6.8.1 em outubro de 2007. 

Como alternativa ao `doesDirectoryExist`, pode-se usar a função `getDirectoryContents` e o tratamento de exceções para verificar se o diretório existe. Mas, em geral, é mais fácil e eficiente usar `doesDirectoryExist` diretamente.

A implementação da função `doesDirectoryExist` depende do sistema operacional. No Unix, por exemplo, ela faz uso da chamada de sistema `stat` para verificar a existência do diretório.

## Veja também

Para mais informações sobre o módulo `System.Directory` e suas funções, consulte a documentação oficial do Haskell: [System.Directory](https://hackage.haskell.org/package/directory-1.3.6.2/docs/System-Directory.html)

Além disso, aqui está um exemplo de como manipular diretórios e arquivos em Haskell: [Tutoriais de manipulação de arquivos em Haskell](https://wiki.haskell.org/Programming_tips/Files)