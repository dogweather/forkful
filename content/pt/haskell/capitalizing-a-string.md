---
title:                "Haskell: Capitalizando uma string"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que

Você já se deparou com uma situação em que precisou capitalizar uma string em um programa Haskell? Talvez você esteja trabalhando em um projeto que requer dados em um formato específico, ou talvez apenas para fins de formatação visual. Independentemente do motivo, é uma tarefa comum em programação e neste artigo vamos explorar como fazer isso de forma simples e eficiente.

## Como Fazer

Primeiro, precisamos entender o que é uma string em Haskell. Em termos simples, uma string é uma sequência de caracteres, como "hello world" ou "1234". Portanto, para capitalizar uma string, precisamos apenas modificar as letras maiúsculas e minúsculas dos caracteres individuais.

Vamos começar com um exemplo básico: capitalizar uma string fornecida pelo usuário.

```Haskell
main :: IO ()
main = do
    putStrLn "Digite uma string: "
    input <- getLine
    let capitalized = map toUpper input
    putStrLn capitalized
```

Este código primeiro solicita ao usuário que digite uma string e armazena a entrada em uma variável chamada "input".  Em seguida, usamos a função "map" para aplicar a função "toUpper" a cada caractere da string, transformando-o em maiúsculo. O resultado é armazenado em uma nova variável chamada "capitalized" e finalmente é impresso na tela.

A saída deste programa seria algo como: "Digite uma string: hello world
HELLO WORLD"

Agora, vamos ver um exemplo de como capitalizar uma string definida pelo programador:

```Haskell
import Data.Char

capitalize :: String -> String 
capitalize [] = []
capitalize (x:xs) = toUpper x : map toLower xs

main :: IO ()
main = do
    let string = "programando em Haskell"
    let capitalized = capitalize string
    putStrLn capitalized
```

Neste exemplo, definimos uma função chamada "capitalize", que recebe uma string e retorna a string capitalizada. Neste caso, usamos as funções "toUpper" e "toLower" da biblioteca padrão de Haskell para transformar a primeira letra em maiúscula e deixar o restante em minúsculas.

A saída deste programa seria: "Programando em haskell".

## Deep Dive

Até agora, vimos duas formas simples de capitalizar strings em Haskell. No entanto, é importante notar que existem outras maneiras de fazer isso e pode ser uma tarefa mais complexa dependendo do contexto.

Por exemplo, se a string contiver símbolos ou acentos, precisaremos tratar esses casos de forma diferente. Além disso, devemos considerar como lidar com espaços em branco no início ou no final da string.

Além disso, existem bibliotecas externas que podem fornecer funções mais avançadas para capitalizar strings, como a biblioteca "text".

Portanto, é importante ter em mente que a capitalização de strings em Haskell pode exigir mais considerações do que apenas aplicar funções básicas "toUpper" e "toLower".

## Veja Também

- [Tutorial básico de Haskell](https://www.haskell.org/tutorial/index.html)
- [Documentação da biblioteca "text"](https://hackage.haskell.org/package/text/docs/Data-Text.html)
- [Exemplos de código para capitalizar strings em Haskell](https://www.tutorialspoint.com/learn_haskell/haskell_strings.htm)