---
title:                "Haskell: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que converter uma string para letras minúsculas?

Conversão de strings para letras minúsculas é uma ação comum necessária em muitos projetos Haskell. Ao converter uma string para letras minúsculas, podemos garantir que os dados inseridos pelo usuário sejam uniformes e padronizados. Também facilita a comparação de strings, já que letras maiúsculas e minúsculas podem afetar o resultado.

## Como fazer:

Para converter uma string para letras minúsculas em Haskell, podemos usar a função `map` combinada com a função `toLower` do módulo `Data.Char`. A função `map` aplica uma função a cada elemento de uma lista, enquanto `toLower` converte um caractere para sua contraparte minúscula. Por exemplo:

```Haskell
import Data.Char
string = "Olá, MUNDO!"
lowercase = map tolower string
putStrLn lowercase
```

A saída deste código seria "olá, mundo!". O caractere 'O' foi convertido para 'o' e todo o restante da string permaneceu inalterado.

## Profundidade:

Ao olhar para a implementação da função `map`, podemos ver que ela funciona recursivamente, o que significa que podemos criar nossa própria função de conversão de strings para letras minúsculas usando recursão. Abaixo está uma possível implementação:

```Haskell
toLowerString :: String -> String
toLowerString [] = []
toLowerString (x:xs) = toLower x : toLowerString xs
```

Nesta função, o primeiro padrão de correspondência (o primeiro elemento é uma lista vazia) é usado como caso base, o que significa que uma lista vazia é retornada. O segundo padrão de correspondência (x:xs) é utilizado para "quebrar" a lista em sua cabeça (primeiro elemento) e a cauda (restante da lista). O caractere da cabeça é convertido para letras minúsculas e a função é aplicada recursivamente à cauda. Esta função nos permite entender melhor como a função `map` funciona e podemos adaptá-la para outras necessidades específicas.

## Veja também:

- [Funções do módulo Data.Char](https://hackage.haskell.org/package/base/docs/Data-Char.html)
- [Recursão na documentação Haskell](https://wiki.haskell.org/Recursion)