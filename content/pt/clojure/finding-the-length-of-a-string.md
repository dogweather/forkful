---
title:    "Clojure: Encontrando o comprimento de uma string"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que?

Encontrar o comprimento de uma string é uma tarefa muito comum em programação, pois permite que os desenvolvedores obtenham informações importantes sobre os dados que estão manipulando. É uma habilidade básica que pode ser aplicada em vários contextos, tornando-se uma ferramenta essencial para qualquer programador.

## Como Fazer

Para encontrar o comprimento de uma string em Clojure, podemos usar a função `count`, que retorna o número de caracteres em uma string. Por exemplo:

```Clojure
(count "Olá, mundo!")
;; Output: 12
```

Outra opção é usar o operador `count` diretamente na string. Por exemplo:

```Clojure
(.length "Olá, mundo!")
;; Output: 12
```

## Profundidade

Ao encontrar o comprimento de uma string, é importante levar em conta alguns detalhes. Em Clojure, strings são sequências de caracteres Unicode, o que significa que nem sempre o número de caracteres corresponderá ao número de bytes. Algumas letras e símbolos do Unicode ocupam mais de um byte, portanto, a função `count` retornará o número total de caracteres, independentemente do número de bytes.

Outra coisa importante a lembrar é que a função `count` funciona em qualquer tipo de sequência, não apenas strings. Portanto, se você passar uma lista ou um vetor como argumento, ele retornará o número de elementos.

## Veja Também

- [Documentação do Clojure sobre função `count`](https://clojuredocs.org/clojure.core/count)
- [Guia de Strings em Clojure](https://clojure.org/guides/strings)