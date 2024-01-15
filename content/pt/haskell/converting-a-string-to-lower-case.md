---
title:                "Convertendo uma string para minúsculas"
html_title:           "Haskell: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que converter uma string para letras minúsculas?

Existem várias situações em que pode ser necessário converter uma string para letras minúsculas no código Haskell. Por exemplo, pode ser útil para manipulação de texto, validação de entrada do usuário ou para comparação de strings sem levar em conta as letras maiúsculas/minúsculas.

## Como fazer

A conversão de uma string para letras minúsculas em Haskell é bastante simples. Você pode utilizar a função `map` juntamente com a função `toLower` do módulo `Data.Char` para aplicar a conversão em cada caractere da string. Veja o exemplo abaixo:

```Haskell
import Data.Char

-- Função que converte uma string para letras minúsculas
toLowerString :: String -> String
toLowerString s = map toLower s
```

Para testar a nossa função, podemos executá-la no console do GHCi, da seguinte forma:

```Haskell
> toLowerString "OLA, MUNDO!"
"ola, mundo!"
```

## Mais informações

Ao usar a função `map` para aplicar a conversão em cada caractere, estamos aproveitando o conceito de função de ordem superior em Haskell. Isso significa que a função `map` toma uma função como um de seus argumentos. E a função `toLower` também é um exemplo de função de ordem superior, pois ela toma um caractere como argumento e retorna outro caractere após a conversão para minúsculas.

Além disso, o módulo `Data.Char` contém outras funções úteis para manipulação de caracteres, como `toUpper` (para conversão para letras maiúsculas) e `isSpace` (para verificar se um caractere é um espaço em branco). Você pode explorá-lo mais para aprofundar seus conhecimentos em Haskell.

## Veja também

- [Documentação oficial do GHC Haskell](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/)
- [Tutorial de Haskell em português](https://haskell.tailorfontela.com.br/)