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

## Por que?

Algumas vezes, precisamos saber o tamanho de uma string em um programa. Isso pode ser necessário para validar dados, realizar operações matemáticas ou simplesmente exibir informações para o usuário.

## Como fazer

Para encontrar o comprimento de uma string em Clojure, podemos usar a função `count`. Por exemplo, se tivermos a seguinte string:

```Clojure
(def s "Olá mundo")
```

Podemos encontrar o seu comprimento usando `count` da seguinte forma:

```Clojure
(count s)
```

Isso nos retorna o valor `10`, pois a string tem 10 caracteres. Podemos também usar a função `count` diretamente em uma string, sem necessidade de armazená-la em uma variável:

```Clojure
(count "Olá mundo")
```

Ambos os exemplos acima retornam o mesmo resultado.

## Aprofundando-se

Além da função `count`, também podemos utilizar `(.length s)` para encontrar o comprimento de uma string. Essa sintaxe é mais semelhante à linguagem Java e pode ser útil para aqueles que estão familiarizados com essa linguagem. No entanto, a função `count` é considerada mais idiomática e é mais comumente usada em Clojure.

Em Clojure, todas as strings são tratadas como sequências, o que significa que podemos usar funções de sequência, como `count`, para encontrar o seu comprimento. Isso também significa que podemos usar outras funções de sequência, como `take`, `drop` e `nth`, em strings.

Portanto, para encontrar o último caractere de uma string, podemos usar a combinação de `count` e `nth`, como mostrado no exemplo abaixo:

```Clojure
(def s "Hello world")
(nth s (dec (count s)))
```

Isso retorna a última letra da string, neste caso, o caractere `"d"`.

## Veja também

- [Documentação da função count em Clojuredocs](https://clojuredocs.org/clojure.core/count)
- [Documentação da função .length em Clojuredocs](https://clojuredocs.org/clojure.core/length)