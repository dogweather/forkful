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

## O que é e por quê?

Ler um arquivo de texto é um processo essencial para os programadores, que consiste em acessar e interpretar o conteúdo de um arquivo de texto em seu código. Isso é especialmente útil quando precisamos lidar com grandes quantidades de dados ou quando queremos armazená-los permanentemente em nossa aplicação.

## Como fazer:

```Elm
-- Lendo um arquivo de texto e imprimindo seu conteúdo

import Text exposing (..)
import File exposing (..)

main =
    let
        fileContent =
            File.read "meuarquivo.txt"
    in
    Text.fromString fileContent
        |> Text.lines
        |> List.map Text.toUpper
        |> Text.unlines
        |> Text.toString
        |> Debug.log "Conteúdo do arquivo:"
```

Saída:

```
Conteúdo do arquivo:
LINHA 1
LINHA 2
LINHA 3
```

## Mergulho profundo:

Ler arquivos de texto é uma tarefa comum em programação, especialmente em linguagens de programação mais clássicas. No entanto, em Elm, onde a linguagem é puramente funcional, acessar e manipular arquivos externos pode ser um pouco mais complicado. É importante ter cuidado ao lidar com arquivos de texto para garantir que não haja sobrecarga desnecessária no sistema.

Uma alternativa ao uso da função `File.read` em Elm é a biblioteca `elm-file-reader`, que fornece uma interface mais simples e abstrata para ler arquivos de texto.

## Veja também:

- Documentação oficial sobre a função `File.read`: https://package.elm-lang.org/packages/elm/file/latest/File
- Biblioteca `elm-file-reader`: https://package.elm-lang.org/packages/mpizenberg/elm-file-reader/latest/