---
title:                "Haskell: Excluindo caracteres que correspondem a um padrão."
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Ao trabalhar com programação em Haskell, é comum precisarmos manipular strings de texto. No entanto, muitas vezes nos deparamos com a necessidade de remover certos caracteres que seguem um determinado padrão. Mas por que isso é necessário? Isso pode acontecer, por exemplo, quando estamos realizando análise de dados e queremos remover caracteres especiais de um conjunto de strings. Ou ainda, quando trabalhamos com entrada de usuários e queremos garantir que apenas caracteres válidos sejam aceitos. Em resumo, a remoção de caracteres que correspondem a um padrão é importante para garantir a integridade dos dados e facilitar seu processamento.

## Como Fazer

O Haskell possui diversas funções para manipulação de strings, entre elas a `delete`. Esta função recebe como parâmetros um caractere e uma string e retorna uma nova string com todas as ocorrências do caractere removidas. Por exemplo, se quisermos remover todos os caracteres "a" de uma string, podemos usar a seguinte expressão:

```Haskell
delete 'a' "banana"
```

O resultado será a string "bnan".

Para remover todos os caracteres que correspondem a um padrão, podemos usar a função `filter`. Esta função recebe como parâmetros uma função de teste e uma lista de elementos. Ela retorna uma nova lista com apenas os elementos que passam no teste da função. No caso de trabalharmos com strings, podemos usar a função `notElem` para verificar se um determinado caractere não faz parte de uma lista de caracteres. Por exemplo, se quisermos remover todas as vogais de uma string, podemos usar a seguinte expressão:

```Haskell
filter (`notElem` "aeiou") "banana"
```

O resultado será a string "bnn".

## Mergulho Profundo

Vale ressaltar que a função `delete` é uma função de ordem superior, ou seja, ela recebe como parâmetro outra função. Isso significa que podemos criar funções mais complexas passando-as como argumento para a função `delete`. Por exemplo, podemos criar uma função que remove todos os caracteres que não são números de uma string:

```Haskell
removeNaoNumeros xs = filter (`notElem` "1234567890") xs
```

Podemos usar essa função junto com a função `delete` para remover apenas os números de uma string:

```Haskell
delete (removeNaoNumeros) "abc123def456"
```

O resultado será a string "abcdef".

Além disso, podemos usar a função `map` em conjunto com a função `delete` para aplicar uma transformação em cada elemento da string antes de removê-los. Por exemplo, se quisermos substituir todos os números por asteriscos, podemos usar a função `map` para substituir cada dígito por um asterisco e depois usar a função `delete` para remover todos os asteriscos da string resultante:

```Haskell
delete (map (\x -> '*') "abc123def456")
```

O resultado será a string "abcdef".

## Veja Também

- [Documentação da função `delete` no Hackage](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-List.html#v:delete)
- [Documentação da função `filter` no Hackage](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:filter)
- [Documentação da função `map` no Hackage](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:map)