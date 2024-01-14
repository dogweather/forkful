---
title:    "Haskell: Excluindo caracteres que correspondem a um padrão"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Muitas vezes, durante a programação em Haskell, podemos nos deparar com a necessidade de deletar caracteres que correspondem a um determinado padrão em uma string ou lista. Isso pode ser útil em várias situações, como por exemplo limpar os dados de entrada ou manipular textos de forma mais eficiente. Neste artigo, vamos explorar como podemos fazer isso de forma simples e eficaz em Haskell. 

## Como Fazer

Em Haskell, podemos utilizar a função `filter` para filtrar elementos de uma lista ou string que atendam a um determinado critério. Vamos supor que queremos deletar todos os dígitos de uma string. Para isso, podemos utilizar a função `isDigit` da biblioteca `Data.Char`, que retorna `True` se o caractere é um dígito e `False` caso contrário. Podemos combinar essa função com a função `not`, que inverte o resultado booleano, para obter um filtro que retorna `True` apenas para caracteres não numéricos.

```Haskell
import Data.Char (isDigit)

stringSemNumeros = filter (\c -> not (isDigit c)) "Abc123deFg456"
```

O código acima irá retornar a string "AbcdeFg". O primeiro argumento da função `filter` é uma função lambda que verifica se o caractere não é um dígito, e o segundo é a string a ser filtrada. Assim, podemos usar essa técnica para deletar qualquer tipo de caractere que desejamos.

## Deep Dive

Em Haskell, podemos usar a notação de lista para representar strings, por exemplo, "ler" pode ser representado como `['l', 'e', 'r']`. Além disso, podemos utilizar operações como `++` para concatenar strings, `[x | x <- string, p x]` para aplicar um filtro `p` em uma string `string` e `[x | x <- string, not (p x)]` para aplicar o inverso do filtro. Todos esses recursos foram usados no exemplo anterior.

Além disso, podemos usar funções de alta ordem para facilitar a criação de filtros mais complexos. Por exemplo, podemos utilizar a função `all` para verificar se todos os elementos da lista atendem a uma determinada condição. No nosso caso, podemos utilizar `all isDigit` para verificar se todos os caracteres da lista são dígitos. Combinando isso com a função `not`, podemos construir um filtro que retorna `True` apenas para strings que não contém dígitos.

```Haskell
stringSemNumeros = filter (\c -> not (all isDigit c)) ["Abc123" , "deFg456"]
```

O código acima irá retornar a lista ["Abc", "deFg"]. Assim, podemos ver que as possibilidades de utilização da função `filter` para deletar caracteres em Haskell são bastante versáteis.

## Veja Também

- [Documentação da função `filter` em Haskell](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:filter)
- [Tutorial de Haskell no portal DevMedia](https://www.devmedia.com.br/aprendendo-haskell-com-ghci/25710)