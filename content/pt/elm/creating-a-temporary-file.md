---
title:                "Elm: Criando um arquivo temporário"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário no Elm?

A criação de arquivos temporários pode ser útil em uma variedade de cenários de programação. Isso inclui a necessidade de armazenar dados temporariamente durante a execução de um programa ou a criação de arquivos que serão excluídos após o uso. No Elm, a criação de um arquivo temporário pode ser uma maneira eficaz de gerenciar e manipular dados temporários.

## Como fazer

Para criar um arquivo temporário no Elm, podemos usar a função `File.Temp.file`. Ela recebe um parâmetro `string` para o nome do arquivo e retorna um `Task` que nos permite manipular o arquivo temporário. Veja o código de exemplo abaixo:

```Elm
import File.Temp
import File
import Task

main : Program Never String
main = 
    File.Temp.file "meu-arquivo.txt" 
        |> Task.andThen manipulateTemporaryFile

manipulateTemporaryFile : File -> Task x ()
manipulateTemporaryFile file = 
    File.write file "Este é um arquivo temporário!"
        |> Task.andThen (\_ -> 
            File.read file
                |> Task.andThen (\content ->
                    -- faça algo com o conteúdo do arquivo temporário
                    Task.succeed content))

```

O código acima primeiro importa os módulos necessários, incluindo `File.Temp` e `File` para manipulação de arquivos. Em seguida, usamos a função `File.Temp.file` para criar um arquivo temporário com o nome "meu-arquivo.txt". Isso retorna um `Task` que permite que usemos a função `Task.andThen` para encadear outras tarefas, como a escrita e leitura do arquivo temporário. O resultado do arquivo é manipulado dentro da função `manipulateTemporaryFile`, onde podemos fazer o que for necessário com ele.

A saída do código acima seria o conteúdo do arquivo temporário, neste caso, "Este é um arquivo temporário!".

## Investigação mais profunda

Criar um arquivo temporário pode ser muito útil, mas também pode ser necessário ter mais controle sobre como e onde o arquivo é armazenado. No Elm, isso pode ser feito usando a função `File.Temp.withName`. Ela aceita um parâmetro adicional para o diretório onde o arquivo temporário deve ser criado.

Além disso, também podemos usar funções adicionais, como `File.Temp.withSettings` para definir configurações específicas para o arquivo temporário, como seu tamanho máximo ou permissões de leitura e gravação.

## Veja também

- https://package.elm-lang.org/packages/elm/file/latest/File-Temp
- https://package.elm-lang.org/packages/elm/file/latest/File
- https://guide.elm-lang.org/error_handling/temp.html