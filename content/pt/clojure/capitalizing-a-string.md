---
title:    "Clojure: Capitalizando uma string"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string em Clojure?

Quando trabalhamos com strings em Clojure, é comum querermos deixá-las em um formato específico. Nesse sentido, a função `capitalize` é uma opção útil para transformar a primeira letra de uma string em maiúscula. Isso é especialmente útil para manter a consistência em seu código ou para apresentar dados em um formato padronizado.

## Como capitalizar uma string em Clojure

Para capitalizar uma string em Clojure, podemos usar a função `capitalize` da seguinte maneira:

```Clojure
(capitalize "exemplo") ; saída: "Exemplo"
(capitalize "uma string") ; saída: "Uma string"
```

Podemos até mesmo capitalizar cada palavra de uma frase usando a função `clojure.string/capitalize`. Para isso, precisamos importar o namespace `clojure.string` com a seguinte sintaxe:

```Clojure
(require '[clojure.string :refer [capitalize]])
```

Em seguida, podemos usar essa função da mesma forma que a `capitalize` comum:

```Clojure
(clojure.string/capitalize "capitalizar cada palavra") ; saída: "Capitalizar Cada Palavra"
```

## Mergulho profundo

Além da função `capitalize`, Clojure também possui outras opções para lidar com strings. Algumas delas são `lower-case` e `upper-case`, que convertem todas as letras de uma string em minúsculas e maiúsculas, respectivamente. Além disso, temos a função `title-case`, que converte a primeira letra de cada palavra em maiúscula, mas mantém o restante em minúsculas.

## Veja também

- [Documentação oficial da função `capitalize`](https://clojuredocs.org/clojure.core/capitalize)
- [Outras funções para trabalhar com strings em Clojure](https://clojuredocs.org/clojure.core#string_functions)