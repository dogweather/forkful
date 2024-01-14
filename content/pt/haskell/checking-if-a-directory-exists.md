---
title:                "Haskell: Verificando se um diretório existe"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que

Em programação, muitas vezes precisamos verificar a existência de um diretório antes de realizar alguma operação de leitura ou escrita nele. Saber se um diretório existe ou não pode ajudar a evitar erros e garantir que nosso código funcione corretamente.

## Como fazer

Para checar se um diretório existe em Haskell, podemos utilizar a função `doesDirectoryExist` do módulo `System.Directory`. Esta função recebe como parâmetro o caminho do diretório que queremos verificar e retorna um valor booleano indicando se o diretório existe ou não.

Vamos ver um exemplo prático:

```Haskell
import System.Directory

main = do
  let path = "/Users/usuario/Documents"
  directoryExists <- doesDirectoryExist path
  if directoryExists
    then putStrLn "O diretório existe!"
    else putStrLn "O diretório nao existe!"
```

Neste exemplo, estamos verificando se o diretório "/Users/usuario/Documents" existe e imprimindo uma mensagem de acordo com o resultado.

## Profundidade

Além da função `doesDirectoryExist`, existem outras funções úteis para trabalhar com diretórios em Haskell, como `createDirectory`, `removeDirectory` e `renameDirectory`. É importante lembrar também que o caminho do diretório que passamos para essas funções deve estar no formato correto para o sistema operacional que estamos usando.

Outro ponto importante é entender como o sistema operacional lida com permissões de diretórios. Se o usuário que está executando o código não tiver permissão para acessar ou modificar o diretório, as funções irão falhar.

## Veja também

- [Documentação oficial do módulo System.Directory](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-Directory.html)
- [Guia de Haskell para iniciantes](https://haskellbr.com/)
- [Tutorial de programação funcional com Haskell](https://www.haskell.org/tutorial/)