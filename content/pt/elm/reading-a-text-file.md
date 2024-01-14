---
title:    "Elm: Lendo um arquivo de texto"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto em Elm?

Algumas vezes, precisamos ler dados de um arquivo de texto para processá-los em nossos programas. Isso pode ser feito de várias maneiras, mas neste artigo, vamos explorar como fazê-lo usando o Elm.

## Como fazer

O Elm possui uma função chamada `File.read` que nos permite ler um arquivo de texto fornecendo o caminho do arquivo. Aqui está um exemplo de código que lê um arquivo chamado `dados.txt` e imprime seu conteúdo no console:

```Elm
module Main exposing (main)

import File exposing (read)

main =
  File.read "dados.txt"
    |> Task.attempt handleResult

handleResult result =
  case result of
    Err _ ->
      "Erro ao ler o arquivo :("
        |> Debug.log "Resultado"
    
    Ok conteudo ->
      conteudo
        |> Debug.log "Resultado"
```

O código acima importa o módulo `File` do Elm e usa sua função `read` para ler o arquivo `dados.txt`. A leitura é realizada como uma `Task`, que é basicamente uma ação que pode ter um resultado bem-sucedido ou um erro. Por isso, é necessário lidar com esses dois possíveis resultados na função `handleResult`. Se a leitura for bem-sucedida, o conteúdo do arquivo será mostrado no console. Caso contrário, uma mensagem de erro será exibida.

Você pode executar este código no Elm Repl e ver o resultado no console. Lembre-se de que, para ler um arquivo de texto no Elm, é necessário que ele esteja no mesmo diretório do arquivo no qual o seu código é executado.

## Mergulho profundo

Se você estiver interessado em entender como exatamente a função `File.read` funciona e como ela lê o arquivo de texto, pode dar uma olhada na documentação do Elm e no código fonte. Ela utiliza a API `FileReader` do JavaScript para realizar a leitura do arquivo em um formato simplificado e seguro.

Além disso, é importante ressaltar que a leitura de arquivos em Elm é uma operação assíncrona, o que significa que você não pode simplesmente ler um arquivo e ter o conteúdo disponível imediatamente. É necessário esperar pelo resultado da `Task` e então lidar com ele.

## Veja também

- Documentação oficial do [File module](https://package.elm-lang.org/packages/elm/file/latest/File)
- Código fonte da função [read do Elm File module](https://github.com/elm/file/blob/1.0.5/src/File.elm#L115)