---
title:                "Lendo um arquivo de texto"
html_title:           "Elm: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto?

Ler arquivos de texto pode ser útil em diversas situações no desenvolvimento de programas em Elm. Seja para acessar informações de um arquivo externo ou para armazenar dados em formato legível, saber como ler e manipular arquivos de texto pode expandir suas habilidades como programador em Elm.

## Como fazer:

A maneira mais simples de ler um arquivo de texto em Elm é utilizando a função `File.fromUrl` combinada com a função `Http.send` do módulo `Http`. Veja o exemplo abaixo:

```
elm package install elm/http
```

```
import Http
import File

url : String
url = "https://meuwebsite.com/conteudo.txt"

lerArquivo : Http.Request File.File
lerArquivo =
  Http.send File.fromUrl url

```

Este código utilizará a função `Http.send` para fazer uma requisição HTTP para o `url` especificado e, em seguida, utilizará a função `File.fromUrl` para converter o conteúdo da resposta em um arquivo de texto. Você pode, então, usar este arquivo em outras funções, como por exemplo:

```
extractContent : File.File -> String
extractContent file =
  File.toString file
    |> Result.withDefault "Erro ao ler o arquivo"

lerConteudo : Cmd msg
lerConteudo =
  lerArquivo
    |> Cmd.map extractContent
    |> Cmd.map MeuTipoMsg
    |> Cmd.batch
```

A função `extractContent` recebe o arquivo lido e o converte em uma string. Você pode usar outras funções do módulo `File` para trabalhar com o arquivo da forma que for mais adequada para sua aplicação.

## Navegação mais profunda:

Além da função `File.fromUrl`, o módulo `File` possui outras funções úteis para trabalhar com arquivos de texto, como por exemplo `File.fromPath` para ler um arquivo local e `File.fromString` para criar um arquivo a partir de uma string. É importante lembrar também de tratar possíveis erros ao ler um arquivo, utilizando funções como `File.toResult` e `Result.withDefault`.

## Veja também:

- Documentação oficial do módulo `File` em Elm: https://package.elm-lang.org/packages/elm/file/latest/
- Tutorial sobre leitura de arquivos em Elm: https://pt.slideshare.net/renamaiotto/elm-read-files
- Exemplo de aplicação em Elm utilizando leitura de arquivos: https://github.com/robwormald/elm-text-editor/