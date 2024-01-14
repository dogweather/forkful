---
title:                "Haskell: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que
Muitas vezes, programadores precisam combinar várias strings em uma só. Isso pode ser útil para criar mensagens personalizadas, montar URLs dinâmicos e muito mais.

## Como fazer
Em Haskell, podemos concatenar strings usando a função "++", que adiciona a segunda string ao final da primeira. Por exemplo:

```Haskell
concatenar :: String -> String -> String
concatenar str1 str2 = str1 ++ str2

concatenar "O meu nome é " "João" -- saída: "O meu nome é João"
```

Podemos até mesmo concatenar mais de duas strings ao mesmo tempo:

```Haskell
concatenarTres :: String -> String -> String -> String
concatenarTres str1 str2 str3 = str1 ++ str2 ++ str3

concatenarTres "Eu sou " "o " "melhor" -- saída: "Eu sou o melhor"
```

## Mergulho profundo
Quando usamos a função "++", o compilador de Haskell converte a primeira string em uma lista de caracteres, adiciona a segunda string no final dessa lista e, em seguida, é gerada uma nova string. Por exemplo, ao concatenar "Hello" e "world", o compilador faz o seguinte:

- Converte "Hello" em uma lista de caracteres: ['H', 'e', 'l', 'l', 'o']
- Adiciona a lista de caracteres de "world" no final dessa lista: ['H', 'e', 'l', 'l', 'o', 'w', 'o', 'r', 'l', 'd']
- Gera uma nova string a partir dessa lista de caracteres: "Hello world"

Isso pode parecer um processo simples, mas é importante ter em mente, principalmente quando tratamos de concatenação de grandes strings em sistemas com limitações de memória.

## Veja também
- [Documentação da função "++" do Haskell](https://www.haskell.org/hoogle/?hoogle=%2B%2B)
- [Tutorial de Haskell para Iniciantes](https://wiki.haskell.org/Haskell_in_5_steps)
- [Listas em Haskell](http://learnyouahaskell.com/starting-out#an-intro-to-lists)

*[Este artigo foi escrito usando a linguagem de programação Haskell](https://www.haskell.org/) e [a sintaxe de Markdown](https://www.markdownguide.org/).]