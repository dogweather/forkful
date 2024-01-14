---
title:                "Haskell: Excluir caracteres que correspondam a um padrão."
simple_title:         "Excluir caracteres que correspondam a um padrão."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que deletar caracteres que correspondem a um padrão?

Existem várias razões pelas quais você pode querer deletar caracteres que correspondem a um padrão em um programa em Haskell. Pode ser parte de um processo de limpeza de dados ou para simplificar strings antes de processá-las. Independentemente do motivo, saber como fazer isso pode economizar muito tempo e esforço.

## Como fazer?

A primeira coisa que precisamos entender é a função `filter`, que é usada para filtrar uma lista de acordo com um predicado. Vamos criar uma pequena lista de caracteres e filtrar apenas aqueles que correspondem ao nosso padrão.

```Haskell
listaCaracteres = "abc123ABC456"
caracteresFiltrados = filter (`elem` "123") listaCaracteres
```

Neste exemplo, estamos usando a função `filter` com o predicado `(`elem` "123")`, que verifica se o caractere está presente na string "123". O resultado será "123456".

No entanto, se quisermos excluir os caracteres que correspondem ao nosso padrão, podemos usar a função `delete` da biblioteca Data.List, que remove a primeira ocorrência de um elemento em uma lista.

```Haskell
import Data.list
listaCaracteres = "abc123ABC456"
caracteresDeletados = delete '2' listaCaracteres
```

O resultado será "abc13ABC456", já que o primeiro "2" encontrado na lista foi removido.

## Aprofundando

Agora que sabemos como usar a função `delete`, podemos usá-la para criar uma função mais geral que deleta todos os caracteres que correspondem a um padrão em uma string.

```Haskell
deletePattern :: [Char] -> String -> String
deletePattern [] s = s
deletePattern (x:xs) s = delete x (deletePattern xs s)
```

Esta função é recursiva e usa a função `delete` para excluir cada caractere da lista de padrões da string. Por exemplo, se quisermos excluir todos os caracteres numéricos de uma string, podemos fazer o seguinte:

```Haskell
deleteNumeros = deletePattern "0123456789"
```

Esta função pode ser facilmente adaptada para excluir caracteres especiais ou qualquer outro padrão desejado.

## Veja também

- [Documentação oficial da função `filter`](https://www.haskell.org/hoogle/?hoogle=filter)
- [Documentação oficial da função `delete`](https://www.haskell.org/hoogle/?hoogle=delete)
- [Tutorial do Learn You a Haskell for Great Good sobre listas](http://learnyouahaskell.com/starting-out#lists)