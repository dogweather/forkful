---
title:                "Elm: Lendo um arquivo de texto"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto?

Ler arquivos de texto é uma habilidade essencial em qualquer linguagem de programação. Em Elm, essa habilidade pode ser especialmente útil para carregar dados dinamicamente em um aplicativo ou para analisar e manipular grandes conjuntos de dados. Neste artigo, vamos explorar como podemos ler um arquivo de texto em Elm e como isso pode ser útil em nossos projetos.

## Como fazer

Para ler um arquivo de texto em Elm, usamos a função `File.read` do módulo `File`. Primeiro, precisamos declarar o tipo de dado que queremos carregar usando a função `text` do módulo `Decode`. Em seguida, usamos a função `File.contents` para ler o conteúdo do arquivo e passamos a função `Decoder.text` como argumento. Por fim, podemos usar o resultado para realizar qualquer manipulação ou exibição que desejarmos.

```
Elm.file
    |> File.read "arquivo.txt"
    |> Task.attempt MyMsg
```

A biblioteca `elm/file` possui outras funções úteis para trabalhar com arquivos, como `File.write` e `File.append`. É importante lembrar que essas funções retornam uma tarefa assíncrona, então precisamos lidar com o resultado usando uma mensagem e a função `Task.attempt`.

## Profundidade

Ao trabalhar com arquivos de texto, é importante lembrar que a carga e manipulação de grandes conjuntos de dados podem afetar o desempenho do nosso aplicativo. Para evitar problemas de desempenho, podemos usar a função `File.bytes` ao invés de `File.text` para ler o arquivo como um conjunto de bytes. Isso é especialmente útil para arquivos grandes, pois evita a conversão de caracteres UTF-8.

Também podemos usar as funções `File.bytes` e `Decoder.bytes` para realizar operações binárias em arquivos, como a leitura de imagens ou outros tipos de dados.

## Veja também

- Documentação oficial do pacote `elm/file`: https://package.elm-lang.org/packages/elm/file/latest/
- Exemplo de leitura de arquivo de texto em Elm: https://ellie-app.com/new
- Tutorial sobre leitura e escrita de arquivos em Elm: https://www.devschool.io/tutoriais/como-salvar-e-ler-arquivos-de-texto-com-elm/