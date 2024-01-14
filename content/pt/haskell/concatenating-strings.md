---
title:                "Haskell: Concatenando strings"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que
Concatenar strings é uma habilidade importante para qualquer programador, pois permite combinar várias cadeias de caracteres em uma única. Isso é útil para criar mensagens dinâmicas, gerar saídas formatadas e muito mais. Além disso, é uma habilidade comum e amplamente utilizada em linguagens de programação como Haskell.

## Como fazer
Para concatenar strings em Haskell, podemos usar o operador `++`. Este operador combina duas strings em uma nova string concatenada. Podemos ver o código abaixo para um exemplo simples:

```
concatena :: String -> String -> String
concatena str1 str2 = str1 ++ str2

resultado = concatena "Olá " "mundo!"

print resultado
-- saída: "Olá mundo!"
```

No código acima, definimos a função `concatena` que recebe duas strings e retorna a concatenação delas usando o operador `++`. Em seguida, usamos essa função para criar a string "Olá mundo!" e imprimir o resultado.

Além do operador `++`, também podemos usar a função `concat` para concatenar uma lista de strings em uma única string. Veja o exemplo abaixo:

```
concatenarLista :: [String] -> String
concatenarLista lista = concat lista

valores = ["Olá", "Programação", "em", "Haskell"]

print (concatenarLista valores)
-- saída: "OláProgramaçãoemHaskell"
```

Neste código, definimos a função `concatenarLista` que recebe uma lista de strings e usa a função `concat` para concatená-las em uma única string. Em seguida, passamos uma lista de valores e imprimimos o resultado.

## Deep Dive
É importante ressaltar que, ao concatenar strings em Haskell, o compilador não irá otimizar o código automaticamente. Cada vez que uma concatenação é feita, uma nova string é criada e a memória alocada para a string anterior será liberada pelo garbage collector. Isso significa que a concatenação repetida de grandes strings pode levar a problemas de desempenho em seu programa.

Para evitar esses problemas, é recomendável usar o tipo de dado `Text` da biblioteca `Data.Text` ao trabalhar com strings maiores. Ele tem uma estrutura interna otimizada que torna a concatenação mais eficiente e menos intensiva em memória.

Outra alternativa é usar a função `concatMap` em vez da função `concat`. Esta função percorre a lista apenas uma vez e concatena as strings de forma mais eficiente. Segue um exemplo:

```
concatenarLista' :: [String] -> String
concatenarLista' lista = concatMap id lista

valores = ["Olá", "Programação", "em", "Haskell"]

print (concatenarLista' valores)
-- saída: "OláProgramaçãoemHaskell"
```

## Veja Também
- [Documentação da biblioteca Data.Text](https://hackage.haskell.org/package/text/docs/Data-Text.html)
- [Tutorial de Haskell no Wikibooks](https://pt.wikibooks.org/wiki/Haskell)
- [Repositório de códigos em Haskell da Microsoft](https://github.com/microsoft/research/tree/master/examples/boogiehaskell/haskell)