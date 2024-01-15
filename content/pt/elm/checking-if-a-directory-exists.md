---
title:                "Verificando se um diretório existe"
html_title:           "Elm: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Porquê

Se você é um programador ou está aprendendo a linguagem Elm, pode se deparar com a necessidade de verificar se um diretório existe. Isso pode ser útil, por exemplo, para garantir que um diretório necessário para a execução de determinada tarefa ou função esteja presente antes de prosseguir no código.

## Como Fazer

Felizmente, a linguagem Elm oferece uma forma simples e eficiente de verificar a existência de um diretório. Para isso, basta utilizar a função `File.System.directoryExists`.

Dentro de um código Elm, a forma de utilizar essa função seria a seguinte:

```
-- define o diretório que será verificado
directory = "caminho/do/diretorio"

-- utiliza a função para verificar se o diretório existe
File.System.directoryExists directory
```

O resultado da execução desse código será um valor booleano `True` ou `False` indicando se o diretório especificado existe ou não. Por exemplo, se o diretório existir, o resultado será `True`, enquanto se o diretório não existir, o resultado será `False`.

## Aprofundando-se

Por trás da função `File.System.directoryExists`, a linguagem Elm se utiliza do módulo `FileSystem` da biblioteca `elm/file` para acessar o sistema de arquivos. Esse módulo oferece diversas outras funções relacionadas à manipulação de arquivos e diretórios, como `File.System.fileExists` para verificar a existência de um arquivo específico ou `File.System.listDirectory` para listar todos os arquivos e diretórios presentes em um determinado diretório.

Além disso, vale ressaltar que a função `File.System.directoryExists` só pode ser utilizada em ambientes que possuem permissão para acessar o sistema de arquivos, como em aplicações de desktop ou em servidores que foram configurados adequadamente.

## Veja Também

- Documentação oficial da função `File.System.directoryExists`: https://package.elm-lang.org/packages/elm/file/latest/File-System#directoryExists
- Documentação oficial do módulo `FileSystem`: https://package.elm-lang.org/packages/elm/file/latest/FileSystem