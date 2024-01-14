---
title:    "Elm: Criando um arquivo temporário"
keywords: ["Elm"]
---

{{< edit_this_page >}}

# Por que criar um arquivo temporário em Elm

Muitas vezes, ao desenvolver um programa em Elm, pode ser necessário criar um arquivo temporário para armazenar informações temporárias. Isso pode ser útil em situações como upload de dados, manipulação de arquivos e outras tarefas que requerem o uso de arquivos temporários.

# Como criar um arquivo temporário em Elm
Para criar um arquivo temporário em Elm, primeiro é necessário usar a função `File.tempFile` que aceita dois argumentos: um prefixo e um sufixo que serão usados ​​para gerar um nome único para o arquivo temporário.

```
Elm 0.19.0

File.tempFile "temp" ".txt"
    |> Task.perform (log "Temporary file created")

```

Utilizamos a função `Task.perform` para executar a tarefa de criação do arquivo temporário e utilizar a função `log` para registrar o nome do arquivo gerado.

A saída do código acima será algo como:

`Temporary file created: temp_84251e.txt`

Com isso, temos um arquivo temporário criado com prefixo "temp" e sufixo ".txt".

# Mergulho profundo
Ao criar um arquivo temporário em Elm, é importante lembrar que esse arquivo será excluído automaticamente assim que o programa for encerrado. Portanto, não é necessário realizar a exclusão manual do arquivo criado.

Além disso, é possível especificar o diretório no qual o arquivo temporário será criado, utilizando a função `File.tempFileIn` que aceita três argumentos: o diretório, o prefixo e o sufixo.

```
Elm 0.19.0

File.tempFileIn "path/to/directory" "temp" ".txt"
    |> Task.perform (log "Temporary file created")

```

# Veja também
- Para mais informações sobre a criação de arquivos em Elm: https://guide.elm-lang.org/interop/file.html
- Documentação da função `File.tempFile`: https://package.elm-lang.org/packages/elm/file/latest/File#tempFile
- Documentação da função `File.tempFileIn`: https://package.elm-lang.org/packages/elm/file/latest/File#tempFileIn