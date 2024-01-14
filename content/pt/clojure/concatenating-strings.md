---
title:    "Clojure: Concatenação de strings"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Por que

A concatenação de strings é uma técnica útil em programação para combinar várias strings em uma única string. Isso pode ser útil para a formação de mensagens, criação de URLs, entre outras tarefas.

## Como fazer

A linguagem de programação Clojure possui uma função simples para concatenar strings, chamada `str`. Por exemplo, para concatenar as strings "olá" e "mundo", podemos usar o código a seguir:

```Clojure
(str "olá" "mundo")
```

Isso resultará na seguinte saída:

```
"olámundo"
```

Podemos até mesmo usar a função `str` para combinar vários tipos de dados, como inteiros e caracteres. Por exemplo, podemos concatenar a string "Meu número favorito é " com o número 6, usando o código a seguir:

```Clojure
(str "Meu número favorito é " 6)
```

E a saída será:

```
"Meu número favorito é 6"
```

## Mergulho profundo

Apesar de ser uma técnica simples, é importante ter cuidado ao concatenar strings em Clojure. Isso porque a linguagem utiliza uma estrutura de dados chamada "immutable persistent data structures", o que significa que as strings não são modificadas diretamente, mas sim criadas como novas strings a partir de partes das originais.

Isso pode causar um impacto na performance do código, especialmente se houver muitas concatenações de strings dentro de um loop, por exemplo. Nesse caso, é recomendado o uso da função `str/join`, que é otimizada para concatenar strings mais eficientemente.

## Veja também

- [A documentação oficial da função `str` em Clojure](https://clojuredocs.org/clojure.core/str)
- [Uma discussão sobre as estruturas de dados imutáveis em Clojure](https://clojure.org/reference/data_structures)
- [Um tutorial detalhado sobre a função `str/join` em Clojure](https://www.baeldung.com/clojure-string-concatenation)