---
title:                "Elm: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto em Elm?

Há muitas razões pelas quais alguém pode querer escrever um arquivo de texto em Elm. Uma das principais vantagens de fazer isso é permitir que suas informações estejam acessíveis e legíveis para outras pessoas, independentemente do sistema operacional em que estão trabalhando. Além disso, escrever um arquivo de texto em Elm permite que você mantenha um registro organizado de seu código, tornando mais fácil o compartilhamento e colaboração com outros programadores.

## Como fazer

Para escrever um arquivo de texto em Elm, você pode seguir alguns passos simples:

1. Comece importando o módulo `File` em seu programa Elm: `import File`.

2. Em seguida, crie uma função que irá lidar com a escrita do arquivo. Você pode chamá-la de `writeFile` ou qualquer outro nome que faça sentido para o seu projeto.

3. Dentro desta função, utilize a função `writeText` do módulo `File`. Esta função leva dois argumentos: o caminho do arquivo onde você deseja escrever e o conteúdo que deseja escrever. Por exemplo: `File.writeText "meuarquivo.txt" "Olá mundo!"`.

4. Em seguida, você precisa criar uma variável que irá armazenar o resultado da função `writeText`. Por exemplo: `let result = File.writeText "meuarquivo.txt" "Olá mundo!"`.

5. Finalmente, você pode utilizar a função `File.file` para acessar os dados resultantes e, se necessário, tratá-los ou exibi-los de alguma forma específica.

Aqui está um exemplo completo de como pode ser o código para escrever um arquivo de texto em Elm:

```Elm
import File

(writeFile : String -> String -> Task x String)
writeFile path content =
    let
        result =
            File.writeText path content
    in
    Task.perform
        (\_ -> "Sucesso!")
        (\_ -> result)

File.file "meuarquivo.txt"
    |> Result.map (\content -> "Conteúdo gravado: " ++ content)
    |> toString
    |> Debug.log "Resultado" -- imprime o resultado no console do navegador
```

Como você pode ver neste exemplo, é possível usar a função `Task` para lidar com operações de E/S assíncronas como a escrita de arquivos.

## Aprofundando um pouco mais

Além de simplesmente escrever um arquivo de texto com conteúdo estático, é possível também criar uma função que leia dados dinamicamente e os escreva em um arquivo. Para isso, você pode utilizar a função `toString` para converter seus dados em uma string e passá-la como parâmetro para a função `writeText`.

Você também pode explorar outras funções do módulo `File`, como `readText` e `appendText`, para ler e adicionar conteúdo a um arquivo existente.

Lembre-se de que é importante tratar os erros em suas operações de E/S, por isso, é recomendável utilizar funções como `Task.andThen` e `Task.onError` para lidar com possíveis falhas.

## Veja também

- [Documentação sobre o módulo `File` em Elm](https://package.elm-lang.org/packages/elm/file/latest/File)
- [Exemplos de uso do módulo `File` em Elm](https://github.com/elm/file/tree/master/examples)
- [Guia para escrever e ler arquivos em Elm](https://medium.com/@MaiaMcCormick/elm-file-systems-parsing-and-writing-d998f92a5028)

Escrever um arquivo de texto em Elm não é uma tarefa complexa, mas pode ser muito útil em diversos projetos. Experimente utilizar a função `writeText` e suas variações em seus próprios programas e veja como pode facilitar o compartilhamento e organização de dados em seu código.