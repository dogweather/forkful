---
title:                "Extraindo substrings"
html_title:           "Bash: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## O Que & Por Que?

Extrair substrings é a ação de obter partes específicas de uma string. Programadores fazem isso frequentemente para manipular dados de string complexos ou grandes.

## Como:

Aqui está um exemplo simples de como extrair substrings em Haskell.

```Haskell
substring :: Int -> Int -> String -> String
substring start end = take (end - start) . drop start
```

Uso e saída:

```Haskell
substring 0 5 "Ola, mundo de Haskell!"  -- Resultado será "Ola, "
```

## Aprofundando

* Histórico: Em Haskell, o ato de extrair substrings tem sido feito usando as funções `drop` e `take` desde seus primórdios. As funções são combinadas para selar a substring dentro do intervalo desejado.
* Alternativas: Outro método é usar a função `splitAt` que divide a string em duas no índice especificado. Mas, dificilmente é preferível em relação à abordagem `take` e `drop` por causa de sua complexidade.
* Detalhes da implementação: Uma coisa importante a notar é que os índices estão baseados em zero e o último índice não está incluso na substring extraída.

## Ver Também

Aprenda mais sobre substrings e strings em Haskell aqui:
* https://wiki.haskell.org/Substring
* https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-String.html