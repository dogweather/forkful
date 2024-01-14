---
title:                "Elm: Criando um arquivo temporário."
simple_title:         "Criando um arquivo temporário."
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

Por que criar um arquivo temporário em Elm?

Criar um arquivo temporário em Elm pode ser útil em várias situações, como armazenar dados temporários ou gerar arquivos para exportação. Além disso, a criação de arquivos temporários pode ajudar a manter o código mais organizado e limpo.

Como criar um arquivo temporário em Elm

Para criar um arquivo temporário em Elm, vamos utilizar a função `File.write`. Essa função recebe como parâmetros a localização do arquivo, os dados que serão escritos e uma função que será executada após a escrita do arquivo. Vamos dar uma olhada no código abaixo:

```Elm
import File
import String exposing (concat)

criarArquivoTemporario : List Int -> Cmd msg
criarArquivoTemporario dados =
    let
        arquivo = "/caminho/do/arquivo/temporario.txt"
        dadosString = concat (List.map toString dados)
        callback _ = Debug.log "Arquivo criado com sucesso"
    in
    File.write arquivo dadosString Ok callback
```

Neste exemplo, estamos criando um arquivo temporário no caminho especificado e escrevendo nele os dados fornecidos na lista. A função `callback` será responsável por retornar uma mensagem com o resultado da criação do arquivo. É importante notar que a função `File.write` retorna um `Cmd msg`, então devemos adicioná-lo ao nosso modelo e utilizá-lo no `update` para que a criação do arquivo seja executada.

Profundizando na criação de arquivos temporários em Elm

Ao criar um arquivo temporário em Elm, é importante estar ciente de que o arquivo será apagado automaticamente após a execução do programa, portanto, não é recomendado utilizá-lo para armazenar dados permanentes. Além disso, também é possível utilizar a função `File.append` para adicionar dados a um arquivo temporário já existente.

Veja também

- Documentação oficial do Elm sobre a função `File.write`: https://package.elm-lang.org/packages/elm/file/latest/File#write
- Tutorial sobre como criar e ler arquivos em Elm: https://thoughtbot.com/blog/creating-and-reading-files-in-elm