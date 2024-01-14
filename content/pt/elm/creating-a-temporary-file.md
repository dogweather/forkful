---
title:    "Elm: Criando um arquivo temporário"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que

Criar arquivos temporários é extremamente útil quando se está lidando com dados ou informações que são necessárias apenas temporariamente. Além disso, é uma boa prática para manter o código organizado e evitar conflitos com outros arquivos existentes.

## Como fazer

Para criar um arquivo temporário em Elm, usamos a função `File.temp` da biblioteca `elm/file`. Vamos criar um arquivo temporário chamado "temp.txt" e escrever algumas informações nele:

```Elm
module Main exposing (main)

import File
import Task

main =
    Task.perform (Result.either onError onSuccess) (File.temp "temp.txt")

onError =
    \error ->
        Debug.log "Error" error

onSuccess =
    \file ->
        File.write file "Este é um arquivo temporário criado em Elm."

```

Ao executar este código, um arquivo temporário será criado na pasta do projeto com o nome "temp.txt" e o conteúdo "Este é um arquivo temporário criado em Elm." será escrito nele.

## Profundidade

Ao criar um arquivo temporário em Elm, é possível especificar o diretório onde o arquivo deve ser criado e o prefixo do nome do arquivo. Por padrão, o diretório será a pasta do projeto e o prefixo será "temp-". Vamos ver um exemplo de como especificar esses parâmetros:

```Elm
Task.perform
    (Result.either onError onSuccess)
    (File.tempWith { directory = "tmp", prefix = "user-" } "info.txt")
```

Neste exemplo, estamos criando um arquivo temporário chamado "info.txt" na pasta "tmp" e com prefixo "user-". Outro parâmetro opcional é o sufixo, que por padrão é ".tmp". Assim, o arquivo temporário criado terá o nome "user-info.tmp".

## Veja também

* Documentação oficial da biblioteca `elm/file`: https://package.elm-lang.org/packages/elm/file/latest/
* Outras maneiras de criar arquivos temporários em Elm: https://discourse.elm-lang.org/t/how-to-create-a-temporary-file/3475