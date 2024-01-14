---
title:                "Clojure: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que usar concatenação de strings em Clojure?

A concatenação de strings é uma técnica essencial em qualquer linguagem de programação, incluindo Clojure. Ela permite que você junte várias strings em uma só, tornando mais fácil a criação de mensagens, strings literais e muito mais.

## Como fazer em Clojure?

Em Clojure, você pode usar a função "str" para concatenar suas strings. Veja como é simples:

```Clojure
(str "Olá" "," "mundo!")
```

Esse código produzirá a seguinte saída:

```
Olá, mundo!
```

Você também pode usar a função "str" para concatenar números, convertendo-os automaticamente em strings:

```Clojure
(str "O número " 42 " é o segredo da vida.")
```

A saída será:

```
O número 42 é o segredo da vida.
```

## Profundidade da concatenação de strings

Embora a função "str" seja a maneira mais comum de concatenar strings em Clojure, existem outras maneiras de fazer isso, dependendo das suas necessidades. Por exemplo, você também pode usar a função "str-join" para concatenar várias strings com um separador:

```Clojure
(str-join ", " ["maçã" "laranja" "banana"])
```

A saída será:

```
maçã, laranja, banana
```

Se você tiver uma lista de strings, também pode usar a função "str-cat" para concatená-las todas juntas:

```Clojure
(str-cat ["Olá" "mundo" "no" "Clojure"])
```

A saída será:

```
OlámundonClojure
```

## Veja também

- [Documentação oficial de concatenação de strings em Clojure](https://clojuredocs.org/clojure.core/str)
- [Tutorial de Clojure para iniciantes](https://www.tutorialspoint.com/clojure/index.htm)
- [Exemplos de código em Clojure](https://github.com/clojure/clojure/tree/master/examples)