---
title:                "Clojure: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que

A capitalização de strings é uma função básica na programação que pode ser útil em várias situações, como na formatação de nomes e títulos em um texto.

## Como Fazer

Para capitalizar uma string em Clojure, podemos utilizar a função `clojure.string/capitalize`. Veja um exemplo abaixo:

```Clojure
(clojure.string/capitalize "hello world")
```

O output deste código seria `Hello World`, com a primeira letra de cada palavra em maiúscula.

## Mais Detalhes

A função `capitalize` utiliza as regras padrão de capitalização da língua inglesa, mas é possível definir regras personalizadas para outras línguas através do uso da função `culture/capitalize`. Além disso, também é possível capitalizar apenas a primeira letra da string usando a função `clojure.string/capitalize-first`. Ambas as funções possuem a mesma sintaxe da função `capitalize` mencionada anteriormente.

## Veja Também

- Documentação oficial do Clojure sobre a função `capitalize`: https://clojuredocs.org/clojure.string/capitalize
- Exemplos de uso da função `capitalize` no site Clojure 101: https://www.clojure101.com/clojure-collection-examples/clojure-string-capitalize