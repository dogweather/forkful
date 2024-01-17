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

## O que & por quê?

O uso de expressões regulares em programação é uma maneira eficiente de realizar a busca e manipulação de padrões de texto em uma string. Programadores frequentemente usam expressões regulares para simplificar o processo de análise e transformação de dados em seus códigos.

## Como fazer:

```
(require '[clojure.string :as str])

;; Função para verificar se um número de telefone é válido:
(defn valid-phone? [phone]
  (if (re-find #"^\(\d{3}\)\s\d{3}-\d{4}$" phone)
    true false))

(str/replace "Olá, meu número é (123) 456-7890" #"(\d{3})\s(\d{3})(-(\d{4}))" "$1-$2$3")

(valid-phone? "(123) 456-7890")
```

Resultado:
```
(123)-456-7890
true
```

## Aprofundando:

### Contexto histórico:
Expressões regulares foram inventadas pelo matemático norte-americano Stephen Kleene na década de 1950 como uma forma de representar padrões em linguagens formais. Sua utilização ganhou popularidade na década de 1970, com o desenvolvimento das linguagens de programação.

### Alternativas:
Embora expressões regulares sejam amplamente utilizadas em programação, existem alternativas, como os módulos de manipulação de strings em diversas linguagens de programação e bibliotecas específicas para processamento de dados, como o Apache Spark.

### Detalhes de implementação:
Em Clojure, as expressões regulares são representadas por um padrão entre ```#""```, incluindo os metacaracteres que definem as regras para a busca de padrões. A função ```re-find``` busca a ocorrência do padrão na string e a função ```str/replace``` substitui a ocorrência pelo novo padrão especificado.

## Veja também:

- Documentação oficial de expressões regulares em Clojure: https://clojuredocs.org/clojure.string/replace
- Livro "Mastering Regular Expressions" de Jeffrey Friedl: https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/
- Tutorial interativo sobre expressões regulares: https://regexone.com/