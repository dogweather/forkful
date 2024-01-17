---
title:                "Encontrando o comprimento de uma string"
html_title:           "Clojure: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O Que e Por Que?

Encontrar o comprimento de uma string é uma tarefa comum em programação. O comprimento de uma string se refere à quantidade de caracteres que compõem essa string. Programadores frequentemente precisam encontrar o comprimento de uma string para realizar outras operações, como validação de entrada de usuário ou manipulação de dados.

## Como Fazer:

Para encontrar o comprimento de uma string em Clojure, podemos usar a função `count`. Essa função recebe uma string como argumento e retorna o número de caracteres nessa string.

```Clojure
(count "Olá, mundo!")
;; output: 12
```

Outra forma de encontrar o comprimento de uma string é usando a função `length`, que recebe uma sequence (como uma string) e retorna o número de elementos dessa sequence.

```Clojure
(length "Olá, mundo!")
;; output: 12
```

## Aprofundando:

Encontrar o comprimento de uma string tem sido uma tarefa importante na programação desde os seus primeiros dias. Antes do surgimento das linguagens de programação modernas, o comprimento de uma string era frequentemente obtido utilizando-se loops e contagem de caracteres. Com o advento de linguagens mais robustas, como Clojure, essa tarefa se tornou mais simples e eficiente.

Existem várias outras formas de encontrar o comprimento de uma string em Clojure, como usando a função `seq`, que retorna uma sequence a partir de uma string e, em seguida, aplicando a função `count` para obter o tamanho dessa sequence. Outra opção é usar a função `str`, que converte qualquer tipo de dado em uma string, e, em seguida, aplicar a função `count`.

## Veja Também:

Para mais informações sobre as funções utilizadas neste artigo, consulte a documentação oficial de Clojure:

- Função `count`: https://clojuredocs.org/clojure.core/count
- Função `length`: https://clojuredocs.org/clojure.core/length
- Função `seq`: https://clojuredocs.org/clojure.core/seq
- Função `str`: https://clojuredocs.org/clojure.core/str