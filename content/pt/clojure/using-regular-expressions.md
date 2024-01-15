---
title:                "Utilizando expressões regulares"
html_title:           "Clojure: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar Expressões Regulares

As expressões regulares são uma ferramenta poderosa para fazer busca e substituição de padrões de caracteres em textos. Elas são úteis para encontrar informações específicas em um grande conjunto de dados e também para validar entradas de usuário em seus programas. Além disso, muitas linguagens de programação, incluindo o Clojure, têm suporte integrado para expressões regulares, tornando-as uma escolha conveniente e eficaz para tarefas de processamento de texto.

## Como usar Expressões Regulares em Clojure

Para usar expressões regulares em Clojure, é necessário importar o módulo `clojure.string`. Em seguida, podemos usar a função `re-find` para encontrar a primeira ocorrência de um padrão em uma string:

```
(require '[clojure.string :as str])

(str/re-find #"foo" "Esta é uma string contendo foo") ; returns "foo"
```

Também é possível fazer substituições usando a função `re-sub`:

```
(str/re-sub #"foo" "bar" "Esta é outra string contendo foo") ; returns "Esta é outra string contendo bar"
```

Uma das maneiras mais versáteis de usar expressões regulares é através da função `re-seq`, que retorna todas as ocorrências de um padrão em uma string em uma sequência:

```
(str/re-seq #"foo" "Esta é uma string com várias ocorrências de foo") ; returns ("foo" "foo")
```

## Mergulho Profundo em Expressões Regulares

Existem várias sintaxes para escrever expressões regulares em Clojure, mas a mais comum é o uso de literais com a notação `#".*"`, onde o padrão é colocado entre aspas duplas. Isso nos permite escrever padrões complexos sem precisar escapar caracteres especiais.

Além disso, podemos usar grupos de captura em expressões regulares para extrair informações específicas de uma string. Por exemplo, se quisermos encontrar todas as ocorrências de um endereço de e-mail em uma string, podemos usar o seguinte padrão:

```
(def texto "Meu endereço de e-mail é exemplo@email.com")

(def matches (str/re-seq #"\b(\w+@\w+(\.\w+)+)\b" texto))

(println matches) ; returns ("exemplo@email.com")
```

Os grupos de captura são delimitados por parênteses e podem ser acessados usando a função `re-matcher` e o método `group`:

```
(let [matcher (re-matcher #"(\d+)-(\d+)-(\d+)" "01-04-2021")]
    [(.group matcher 1) ; retorna "01"
     (.group matcher 2) ; retorna "04"
     (.group matcher 3)]) ; retorna "2021"
```

## Veja Também

- [Documentação oficial sobre Expressões Regulares em Clojure](https://clojuredocs.org/clojure.string/re_find)
- [Tutorial interativo sobre Expressões Regulares em Clojure](https://regexone.com/references/clojure)