---
title:                "Criando um arquivo temporário"
html_title:           "Elm: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que

Você já precisou criar um arquivo temporário em um programa? Criar arquivos temporários é uma tarefa essencial em muitos projetos de programação, seja para armazenar dados temporários ou para realizar alguma operação específica.

## Como Fazer

Para criar um arquivo temporário em Elm, podemos utilizar a biblioteca `elm/file`:

```elm
import File

createTempFile : Task x File.Handle
createTempFile =
  File.tempFile "my-temp-file.txt"

```

O código acima utiliza a função `tempFile` da biblioteca `File` para criar um arquivo temporário chamado "my-temp-file.txt". A função retorna um `Task` que irá gerar um `File.Handle`, que pode ser usado para escrever e ler no arquivo.

Podemos então utilizar o `Task` retornado para lidar com o arquivo temporário:

```elm
import File
import Task exposing (andThen)

createAndWriteTempFile : String -> Task x ()
createAndWriteTempFile content =
  File.tempFile "my-temp-file.txt"
    |> andThen (\handle -> File.write handle content)

```

No exemplo acima, usamos a função `andThen` da biblioteca `Task` para encadear a criação do arquivo temporário com a escrita de conteúdo nele.

## Deep Dive

Por padrão, os arquivos temporários criados com a função `tempFile` serão salvos no diretório temporário do sistema operacional do usuário. Porém, é possível especificar um diretório específico para salvar o arquivo temporário utilizando a função `tempFileIn`:

```elm
import File

createTempFileInHomeDirectory : Task x File.Handle
createTempFileInHomeDirectory =
  File.tempFileIn "/home/user/" "my-temp-file.txt"

``` 

Outro aspecto importante a se destacar é que os arquivos temporários geralmente são excluídos automaticamente pelo sistema operacional após serem utilizados. No entanto, se necessário, é possível explicitamente excluir um arquivo temporário utilizando a função `delete` da biblioteca `File`:

```elm
import File
import Task exposing (andThen)

deleteTempFile : Task error ()
deleteTempFile =
  File.tempFile "my-temp-file.txt"
    |> andThen File.delete

```

## Veja Também

- [Documentação oficial da biblioteca `elm/file`](https://package.elm-lang.org/packages/elm/file/latest/)
- [Guia completo de como criar, ler e escrever arquivos em Elm](https://medium.com/@TylerEich/elm-file-io-b56a09a94093)