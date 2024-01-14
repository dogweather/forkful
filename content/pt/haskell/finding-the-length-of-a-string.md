---
title:                "Haskell: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Existem muitos cenários em que precisamos descobrir o comprimento de uma string em Haskell, desde validar inputs do usuário até manipular texto em um programa. Saber como encontrar o comprimento de uma string é uma habilidade essencial para qualquer programador em Haskell.

## Como fazer

Existem algumas maneiras diferentes de encontrar o comprimento de uma string em Haskell. Vamos começar com a abordagem mais simples, usando a função `length`:

```
Haskell> length "Olá mundo!"
11
```

Essa função é uma das funções de alta ordem mais básicas em Haskell e é definida da seguinte maneira:

```
length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs
```

A função `length` recebe uma lista e retorna o comprimento dessa lista como um número inteiro. A primeira linha da definição de tipo mostra que essa função é polimórfica e pode lidar com listas de qualquer tipo.

Além da função `length`, existem outras maneiras de encontrar o comprimento de uma string em Haskell. Uma opção é usar a função `foldl`, que pode ser implementada da seguinte maneira:

```
myLength :: [a] -> Int
myLength xs = foldl (\acc x -> acc + 1) 0 xs
```

A função `foldl` reduz uma lista aplicando uma função a cada elemento e acumulando o resultado. Neste caso, a função simplesmente adiciona 1 ao acumulador a cada iteração, resultando no comprimento final da lista.

## Mergulho profundo

Ao usar a função `length`, é importante ter em mente que ela percorre a lista inteira, o que pode ser um problema para listas longas. Uma solução para isso é usar a função `foldl'` do módulo `Data.List`, que é otimizada para evitar chamadas recursivas desnecessárias.

Outro ponto importante é a diferença entre strings e listas de caracteres em Haskell. Em Haskell, uma string é simplesmente uma lista de caracteres, portanto, as funções de lista, como `length` e `foldl`, podem ser usadas diretamente em strings.

Além disso, é possível usar a sintaxe de lista de compreensão com strings em Haskell, tornando mais fácil para os programadores trabalhar com elas. Por exemplo, podemos encontrar o comprimento de uma string usando a seguinte expressão:

```
lengthStr :: String -> Int
lengthStr str = sum [1 | _ <- str]
```

## Veja também

- [Documentação do Haskell](https://www.haskell.org/documentation/)
- [Funções de alta ordem em Haskell](https://wiki.haskell.org/Function)
- [Manipulação de strings em Haskell](https://wiki.haskell.org/Strings_as_lists)