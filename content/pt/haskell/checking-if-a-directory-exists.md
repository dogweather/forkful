---
title:    "Haskell: Verificando se um diretório existe"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe

Ao escrever programas em Haskell, é importante ter o controle de quais diretórios existem em um determinado sistema de arquivos. Isso pode ser útil quando se trabalha com arquivos e pastas, pois permite que o programa execute certas ações com base na existência de um diretório.

## Como fazer

A verificação da existência de um diretório em Haskell pode ser feita usando a função `doesDirectoryExist` da biblioteca `System.Directory`. Aqui está um exemplo de código e saída:

```
```Haskell
import System.Directory

main = do
  dirStatus <- doesDirectoryExist "pasta"
  if dirStatus
    then putStrLn "O diretório existe"
    else putStrLn "O diretório não existe"
```
```
Saída:
```
O diretório existe
```
Neste exemplo, a função `doesDirectoryExist` foi usada para verificar se o diretório "pasta" existe. Se o diretório existir, a mensagem "O diretório existe" será impressa na tela. Caso contrário, a mensagem "O diretório não existe" será exibida.

## Profundidade

O processo de verificação da existência de um diretório em Haskell envolve mais do que apenas chamar a função `doesDirectoryExist`. Internamente, essa função usa a função `access` do sistema operacional para verificar se o diretório existe ou não. Além disso, existem outras funções relacionadas que podem ser úteis, como `createDirectory` e `removeDirectory`, que permitem criar ou remover um diretório, respectivamente.

Ao trabalhar com diretórios, é importante considerar as permissões de acesso do usuário que está executando o programa. Se o usuário não tiver as permissões necessárias, as funções de verificação e manipulação de diretórios podem falhar.

## Veja também

- [Documentação da função `doesDirectoryExist` em Haskell](https://hackage.haskell.org/package/directory/docs/System-Directory.html#v:doesDirectoryExist)
- [Documentação da função `access` do sistema operacional](https://man7.org/linux/man-pages/man2/access.2.html)
- [Artigo sobre manipulação de arquivos e diretórios em Haskell](https://wiki.haskell.org/Handling_files)