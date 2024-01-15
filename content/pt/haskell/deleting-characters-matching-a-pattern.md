---
title:                "Exclusão de caracteres correspondentes a um padrão"
html_title:           "Haskell: Exclusão de caracteres correspondentes a um padrão"
simple_title:         "Exclusão de caracteres correspondentes a um padrão"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Por que
Algumas vezes precisamos limpar uma string removendo caracteres que correspondem a um padrão específico. Por exemplo, em uma aplicação de formulário, se o usuário digitar um número de telefone com parênteses, traços ou espaços, podemos querer remover esses caracteres antes de armazená-lo no banco de dados.

## Como Fazer

Para deletar caracteres a partir de um padrão em Haskell, podemos utilizar a função `filter`. Essa função recebe como parâmetros um predicado (uma função que retorna `True` ou `False`) e uma lista. O predicado é aplicado a cada elemento da lista e somente os elementos que retornarem `True` serão mantidos na lista resultante.

O código a seguir remove todos os números de uma string:

```Haskell
removeNumeros :: String -> String
removeNumeros str = filter (not . isDigit) str
```

No exemplo acima, usamos a função `isDigit` da biblioteca `Data.Char` para verificar se um caractere é um número. O `.` é uma função de composição, que neste caso combina a função `not` (que inverte o resultado de uma função booleana) com a função `isDigit`.

Podemos agora usar a função `removeNumeros` para remover números de uma string:

```Haskell
removeNumeros "abc123def"
-- "abcdef"
```

Se quisermos remover vários tipos de caracteres, podemos utilizar a função `isAlphaNum` que verifica se um caractere é alfanumérico (letras ou números). Podemos também criar nossa própria função predicado.

```Haskell
removeCaracteres :: String -> [Char] -> String
removeCaracteres str chars = filter (`notElem` chars) str
```

A função acima recebe uma string e uma lista de caracteres e remove todos os caracteres que estão presentes na lista. Usando essa função, podemos remover parênteses, traços e espaços de uma string:

```Haskell
removeCaracteres "(12) 345-6789" "()- "
-- "123456789"
```

## Deep Dive

A função `filter` é uma das funções de ordem superior em Haskell, ou seja, uma função que recebe outra função como parâmetro. Isso permite que a função `filter` seja usada de forma mais genérica, por exemplo, para filtrar elementos em uma lista que atendem a um certo critério.

Para entender melhor como a função `filter` funciona, podemos reproduzi-la de forma simplificada:

```Haskell
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x       = x : filter' p xs
  | otherwise = filter' p xs
```

Essa função tem como parâmetros um predicado `p` e uma lista de elementos do tipo `a`. Se a lista estiver vazia, a função retorna uma lista vazia. Caso contrário, a função verifica se o primeiro elemento da lista atende ao predicado `p`. Se sim, esse elemento é adicionado à lista resultante, caso contrário, ele é ignorado. Em seguida, a função é chamada recursivamente para o restante da lista. Isso se repete até que todos os elementos sejam verificados.

Um detalhe importante é que o tipo do predicado `p` é `(a -> Bool)`, ou seja, uma função que recebe um elemento do tipo `a` e retorna um valor booleano. Isso significa que podemos passar qualquer função que atenda a esse tipo como argumento para a função `filter`. Podemos até mesmo utilizar funções anônimas para criar predicados mais complexos.

Por exemplo, podemos usar `filter` para remover todos os números pares de uma lista:

```Haskell
filter even [1..10]
-- [2,4,6,8,10]
```

Nesse caso, `even` é uma função que verifica se um número é par.

## Veja Também

- [Funções em Haskell](https://wiki.haskell.org/Function)
- [Compreensão de Listas em Haskell](https://wiki.haskell.org/List_comprehension)
- [Documentação da biblioteca `Data.Char`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html)