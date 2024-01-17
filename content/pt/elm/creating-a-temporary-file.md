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

## O que é e por que fazemos isso?

Ao desenvolver software, muitas vezes os programadores precisam criar arquivos temporários para armazenar dados temporariamente. Isso pode ser necessário para diversas tarefas, como realizar testes, armazenar informações temporárias em um programa ou até mesmo para facilitar a organização do código.

## Como fazer:

Em Elm, podemos criar um arquivo temporário usando a função `File.temp` do pacote `elm/file`. Essa função recebe como argumento uma string com o nome do arquivo e retorna um `Result` contendo o caminho completo do arquivo temporário. Veja um exemplo:

```Elm
import File

fileResult : Result File.Error String
fileResult =
  File.temp "meu-arquivo-temporario.txt"
```

Vale lembrar que, caso o arquivo já exista, a função retornará um erro. Para isso, podemos usar a função `File.tempWith` que recebe um segundo argumento como uma função de `Result` para lidar com possíveis erros. Por exemplo:

```Elm
import File

saveFile : String -> Result File.Error String
saveFile name =
  File.tempWith (\err -> Err "Arquivo temporário já existe") name
```

## Deep Dive:

Criar arquivos temporários é uma prática comum na programação e pode ser usada em várias linguagens de programação, não apenas em Elm. Algumas delas possuem funções e pacotes específicos para essa finalidade, como é o caso do Node.js com o pacote `fs` e do Python com a função `tempfile`. 

Além disso, existem algumas alternativas para criar arquivos temporários em Elm sem utilizar a função `File.temp`. Por exemplo, é possível usar a função `Http.send` para fazer uma requisição a um servidor e receber uma resposta com um arquivo temporário. Outra opção é usar a biblioteca `elm-fs` que oferece várias funções para trabalhar com arquivos.

## Veja também:

- Documentação da função [File.temp](https://package.elm-lang.org/packages/elm/file/latest/File#temp) em Elm.
- Outras funções para lidar com arquivos em Elm na biblioteca [elm/file](https://package.elm-lang.org/packages/elm/file/latest/File).
- Fonte [fs do Node.js](https://nodejs.org/api/fs.html) e função [tempfile do Python](https://docs.python.org/3/library/tempfile.html).