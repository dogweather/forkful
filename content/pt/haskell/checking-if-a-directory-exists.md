---
title:                "Haskell: Verificar se um diretório existe."
simple_title:         "Verificar se um diretório existe."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe em Haskell?

Quando estamos trabalhando com programação, muitas vezes precisamos manipular arquivos e diretórios. Mas e se precisarmos verificar se um determinado diretório realmente existe em nosso sistema? É nessa situação em que a função `doesDirectoryExist` em Haskell torna-se extremamente útil. Ela nos permite verificar se um caminho de diretório fornecido é válido ou não, o que pode nos poupar tempo e evitar possíveis problemas em nosso código.

## Como verificar se um diretório existe em Haskell

Antes de começarmos a usar a função `doesDirectoryExist`, é importante importar o módulo `System.Directory`. Assim, podemos utilizar as funções do módulo para verificar diretórios e realizar outras operações relacionadas a arquivos.

Vamos imaginar que queremos verificar a existência do diretório "/home/usuario/meuProjeto". Podemos fazer isso da seguinte maneira:

```Haskell
import System.Directory

main = do
    dirExist <- doesDirectoryExist "/home/usuario/meuProjeto"
    if dirExist
        then putStrLn "O diretório existe"
        else putStrLn "O diretório não existe"
```

Ao executar o código acima, obteremos o seguinte resultado:

```Shell
O diretório existe
```

Por outro lado, se mudarmos o caminho para um diretório inexistente, receberemos o seguinte resultado:

```Shell
O diretório não existe
```

É importante ressaltar que a função `doesDirectoryExist` só pode verificar diretórios e não arquivos. Para verificar se um arquivo existe, podemos utilizar a função `doesFileExist` do mesmo módulo.

## Explorando mais a função `doesDirectoryExist`

Além de nos ajudar a verificar a existência de um diretório, a função `doesDirectoryExist` também pode ser combinada com outras funções do módulo `System.Directory` para realizar tarefas mais complexas. Por exemplo, podemos listar os arquivos de um determinado diretório com a função `listDirectory` e, em seguida, verificar a existência de cada um desses arquivos utilizando a função `doesFileExist`.

Outra informação importante é que a função `doesDirectoryExist` retorna um valor do tipo `IO Bool`, o que significa que ela precisa ser executada dentro de uma ação `do`. Além disso, ela lança uma exceção caso ocorra algum erro durante a execução, por isso é importante estar ciente disso ao utilizar a função em nosso código.

## Veja também

- [Documentação oficial da função doesDirectoryExist em Haskell](https://hackage.haskell.org/package/directory/docs/System-Directory.html#v:doesDirectoryExist)
- [Manipulação de arquivos e diretórios em Haskell](https://wiki.haskell.org/Introduction_to_IO)