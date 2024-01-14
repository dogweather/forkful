---
title:                "Elm: Escrevendo um arquivo de texto"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto em Elm?

Se você é um programador em potencial ou já trabalha com linguagem de programação, é importante considerar o uso do Elm. Escrever um arquivo de texto em Elm pode ser um processo simples e eficaz para criar e organizar informações em um formato legível por máquina.

## Como escrever um arquivo de texto em Elm

Para escrever um arquivo de texto em Elm, você precisará seguir alguns passos simples:

1. Crie uma função `criarArquivo` que aceite uma string como parâmetro.
2. Use a função `writeString` para escrever a string em um arquivo.
3. Defina o nome e local do arquivo usando a função `name` e `directory`.
4. Chame a função `criarArquivo` passando a string desejada como argumento.

Aqui está um exemplo de código em Elm para criar um arquivo de texto e escrever a string "Olá, mundo!" nele:

```Elm
import File
import Task exposing (Task)
import Task exposing (andThen)

criarArquivo : String -> Task x ()
criarArquivo text =
    File.write (File.name "arquivo.txt" (File.directory "./arquivos")) text

main : Task x ()
main =
    criarArquivo "Olá, mundo!"
```

Este código criará o arquivo "arquivo.txt" no diretório "arquivos" e escreverá a string nele. Você pode verificar o arquivo criado e confirmar que a string foi gravada corretamente.

## Profundidade no processo de escrita de um arquivo de texto em Elm

Escrever um arquivo de texto em Elm é uma operação de entrada e saída (I/O), o que significa que você precisa considerar alguns aspectos ao lidar com ela. Por exemplo, a função `File.write` retorna uma tarefa (task), que é uma operação assíncrona. Como resultado, você precisa lidar com o fluxo de controle através de funções `andThen` e `catch`.

Além disso, você pode tornar o processo de escrita de um arquivo mais complexo, modificando o código para permitir a escrita de mais de uma string em diferentes arquivos. Isso requer uma abordagem de programação mais avançada, mas é possível graças ao tipo de dado `File.Handle`.

## Veja também

Aqui estão alguns links úteis para aprender mais sobre escrever arquivos de texto em Elm:

- Documentação oficial do Elm: https://guide.elm-lang.org/
- Perguntas frequentes sobre Elm: https://elmprogramming.com/faq
- Comunidade de programação Elm: https://discourse.elm-lang.org/