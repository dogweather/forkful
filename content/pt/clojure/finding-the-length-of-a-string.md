---
title:                "Clojure: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Por que encontrar o comprimento de uma string em Clojure?

Encontrar o comprimento de uma string é uma tarefa comum em programação que pode ser útil para várias finalidades, como validar a entrada do usuário ou contar o número de caracteres de uma mensagem. Em Clojure, é fácil e simples encontrar o comprimento de uma string. Neste artigo, exploraremos como fazer isso passo a passo.

## Como fazer

Para encontrar o comprimento de uma string em Clojure, podemos usar a função `count`, que é nativa da linguagem e retorna o número de elementos em uma coleção.

```Clojure
(def s "Olá mundo!")
(count s)
```

A saída será `11`, pois a string contém 11 caracteres.

Outra opção é usar a função `str` em conjunto com `count`. A função `str` retorna uma string contendo os argumentos fornecidos, e podemos passar apenas a string desejada para obter seu comprimento.

```Clojure
(def s "Olá mundo!")
(count (str s))
```

A saída é a mesma, `11`.

### Tratando espaços em branco

É importante notar que a função `count` inclui os espaços em branco na contagem do comprimento da string. Se quisermos desconsiderá-los, podemos usar a função `clojure.string/trim` para remover os espaços antes de contar.

```Clojure
(def s "  Olá mundo!  ")
(count (clojure.string/trim s))
```

Agora, a saída será `9`, já que os espaços em branco foram removidos.

## Profundidade

Ao lidar com string em Clojure, é interessante saber que a linguagem trata strings como sequências de caracteres, o que significa que podemos aplicar funções de sequência a elas. Por exemplo, podemos usar a função `map` para aplicar uma função a cada caractere da string.

```Clojure
(def s "Olá mundo!")
(map char s)
```

A saída será uma sequência de caracteres, `[O l á  m u n d o !]`.

Além disso, como strings são sequências, podemos usar índices para acessar caracteres específicos. Por exemplo, o segundo caractere da string é acessado usando `s1`, já que em Clojure os índices começam em 0.

```Clojure
(def s "Olá mundo!")
(s 1)
```

A saída será `l`.

# Veja também

- Documentação oficial de `count`: https://clojuredocs.org/clojure.core/count
- Documentação oficial de `str`: https://clojuredocs.org/clojure.core/str
- Documentação oficial de `map`: https://clojuredocs.org/clojure.core/map
- Documentação oficial de `char`: https://clojuredocs.org/clojure.core/char
- Documentação oficial de `trim`: https://clojuredocs.org/clojure.string/trim