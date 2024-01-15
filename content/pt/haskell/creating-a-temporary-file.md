---
title:                "Criando um arquivo temporário"
html_title:           "Haskell: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que

Você pode se perguntar por que alguém iria querer criar um arquivo temporário em um programa Haskell. A resposta é simples: às vezes, precisamos armazenar informações temporariamente em um arquivo antes de processá-las ou manter o estado atual do programa. Criar um arquivo temporário é uma maneira eficiente e segura de fazer isso.

## Como

Para criar um arquivo temporário em Haskell, utilizamos a função `withTempFile` do módulo `System.IO.Temp`. Esta função recebe dois parâmetros: o diretório no qual o arquivo temporário deve ser criado e um prefixo para o nome do arquivo.

Vamos ver um exemplo simples:

```
import System.IO.Temp

main :: IO ()
main = withTempFile "." "temp" $ \tempFile handle -> do
    writeFile tempFile "Hello world!"
    contents <- readFile tempFile
    putStrLn contents
```

Neste exemplo, estamos criando um arquivo temporário no diretório atual com o prefixo "temp". Em seguida, escrevemos a string "Hello world!" no arquivo e, finalmente, lemos e imprimimos seu conteúdo. Ao final da execução do programa, o arquivo temporário é apagado automaticamente.

O segundo parâmetro da função `withTempFile` é uma função de callback que recebe dois argumentos: o caminho do arquivo temporário e um `Handle` que pode ser usado para escrever e ler dados no arquivo.

## Deep Dive

Internamente, a função `withTempFile` utiliza a função `openTempFile`, que pode ser usada diretamente caso você precise de mais controle sobre o processo de criação do arquivo temporário. Esta função recebe os mesmos parâmetros que `withTempFile` e retorna um `IO (FilePath, Handle)` contendo o caminho do arquivo e o `Handle`.

Além disso, o módulo `System.IO.Temp` também possui outras funções úteis para manipular arquivos temporários, como `openBinaryTempFile` e `createTempDirectory`.

## Veja também

- [Documentação do módulo `System.IO.Temp`](https://hackage.haskell.org/package/base/docs/System-IO-Temp.html)
- [Tutorial de introdução ao Haskell](https://haskell.org/tutorial/)