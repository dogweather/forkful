---
title:                "Extraindo substrings"
html_title:           "Elm: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que 

Você já teve que lidar com uma string longa e precisou extrair apenas uma parte específica dela? Ou talvez você esteja procurando maneiras mais eficientes de trabalhar com textos em seus projetos Elm. Independentemente do motivo, saber como extrair substrings pode ser útil ao lidar com strings em suas aplicações.

## Como fazer

Para extrair substrings em Elm, podemos usar a função `slice` que faz parte do módulo `String`. Esta função recebe três parâmetros: o índice inicial, o índice final e a string de origem.

Por exemplo, se tivermos a string "Olá mundo!", podemos usar `slice 0 3 "Olá mundo!"` para extrair a substring "Olá". Isso porque o primeiro parâmetro representa o índice inicial (que começa em 0), enquanto o segundo parâmetro representa o índice final (que é exclusivo, ou seja, o caractere no índice final não será incluído na substring).

Outra forma de extrair substrings é usando a função `substring` que também está presente no módulo `String`. Esta função recebe dois parâmetros: o índice inicial e a string de origem. A partir do índice inicial, a substring será extraída até o final da string de origem.

Por exemplo, se usarmos `substring 4 "Olá mundo!"`, iremos extrair a substring "mundo!" da string de origem "Olá mundo!".

Aqui está um exemplo prático do uso dessas funções em Elm:

```
module Main exposing (main)

import Html exposing (text)
import String

-- Extraindo substrings com a função slice
substring1 = String.slice 0 4 "Olá mundo!"
substring2 = String.slice 5 11 "Olá mundo!"

-- Extraindo substrings com a função substring
substring3 = String.substring 4 "Olá mundo!"

main = 
  text (substring1 ++ " " ++ substring2 ++ " " ++ substring3)
```

A saída deste exemplo será "Olá mund mundo!". Separamos a saída com espaços para ficar mais claro qual substring foi extraída em cada etapa.

## Deep Dive

Agora que você sabe como extrair substrings em Elm, vamos dar uma olhada em alguns detalhes importantes.

Primeiro, é importante destacar que, assim como na maioria das linguagens de programação, os índices em Elm começam em 0. Ou seja, o primeiro caractere de uma string terá o índice 0, o segundo caractere terá o índice 1 e assim por diante.

Além disso, como mencionado anteriormente, o índice final em `slice` é exclusivo, o que significa que o caractere no índice final não será incluído na substring. Portanto, se quisermos extrair a primeira palavra da string "Hello, world!", devemos usar `String.slice 0 5 "Hello, world!"` ao invés de `String.slice 0 4 "Hello, world!"`.

Outro detalhe importante a ser mencionado é que as funções `slice` e `substring` retornam uma `Maybe String`, que é basicamente um valor que pode ser `Just value` ou `Nothing` em Elm. Isso significa que se o índice inicial ou final fornecido estiver fora do intervalo da string de origem, o valor retornado será `Nothing`. É importante estar ciente disso ao lidar com os resultados dessas funções.

## Veja também

- Documentação oficial de `String.slice`: https://package.elm-lang.org/packages/elm/core/latest/String#slice 
- Documentação oficial de `String.substring`: https://package.elm-lang.org/packages/elm/core/latest/String#substring