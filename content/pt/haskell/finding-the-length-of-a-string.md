---
title:                "Encontrando o comprimento de uma string"
html_title:           "Haskell: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Você já precisou saber quantos caracteres existem em uma palavra ou frase? Saber a extensão de uma string pode ser útil em várias situações, especialmente na manipulação de texto. Neste artigo, vamos explorar como encontrar o comprimento de uma string em Haskell.

## Como fazer

Encontrar o comprimento de uma string em Haskell é bem simples e pode ser feito de diferentes maneiras. Aqui, vamos abordar três métodos: usando a função `length`, recursão e list comprehension.

### Usando a função `length`

A função `length` é uma função nativa em Haskell que retorna o comprimento de uma lista, incluindo strings. Basta passar a string como argumento para a função e ela irá retornar o número de caracteres.

```Haskell
length "Olá, mundo!" -- retorna 12
```

### Usando recursão

A recursão é uma técnica muito comum em programação funcional e também pode ser utilizada para encontrar o comprimento de uma string em Haskell. Essencialmente, a ideia é percorrer a string caracter por caracter, incrementando um contador a cada iteração até que se chegue ao final da string.

```Haskell
strLength :: String -> Int
strLength [] = 0 -- caso base, string vazia
strLength (x:xs) = 1 + strLength xs -- recursão, incrementa 1 e passa para o próximo caractere

strLength "Olá, mundo!" -- retorna 12
```

### Usando list comprehension

Outra forma de encontrar o comprimento de uma string é usando list comprehension. Nesse caso, é criada uma lista com os caracteres da string e então é utilizado o operador `length` para obter o tamanho da lista.

```Haskell
strLength :: String -> Int
strLength str = length [x | x <- str] -- cria uma lista com os caracteres e aplica a função `length`

strLength "Olá, mundo!" -- retorna 12
```

## Deep Dive

Embora possa parecer simples encontrar o comprimento de uma string em Haskell, é importante entender como as diferentes abordagens funcionam por trás dos panos.

No caso da função `length`, ela utiliza o conceito de *fold* (dobra) para percorrer a lista e contar seus elementos. Já na recursão, estamos trabalhando com o conceito de *pattern matching* (casamento de padrões) para lidar com diferentes casos, como a string vazia. E no list comprehension, estamos criando uma nova lista baseada nos elementos da string e utilizando a função `length` para encontrar o tamanho dessa lista.

Além do mais, é interessante observar que, em Haskell, uma string é simplesmente uma lista de caracteres, o que facilita o seu tratamento por meio de funções nativas ou funcionalidades mais elaboradas.

## Veja também

- [Documentação oficial do Haskell](https://www.haskell.org/documentation/)
- [Haskell Brasil](https://www.haskellbr.com)
- [Haskell no Reddit](https://www.reddit.com/r/haskell)