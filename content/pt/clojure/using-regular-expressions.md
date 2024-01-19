---
title:                "Usando expressões regulares"
html_title:           "Gleam: Usando expressões regulares"
simple_title:         "Usando expressões regulares"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O Que e Por Que?

Expressões regulares (regex) são padrões usados para encontrar combinações correspondentes em strings. Os programadores as utilizam para manipular, analisar e validar dados de texto, economizando tempo e esforço.

## Como se faz:

Aqui está como você pode usar expressões regulares em Clojure:

```Clojure
(def frase "Bem-vindo ao mundo de Clojure.")

if(print (re-seq #"o" frase))
  ; => ("o" "o" "o")

(defn validar-email [email]
  (boolean (re-matches #".+@.+\\..+" email)))

if(print (validar-email "meu.email@teste.com"))
  ; => true
```

Neste caso, re-seq é usado para encontrar todas as correspondências de "o" na frase e re-matches para validar um formato de email.

## Mergulho Profundo

As expressões regulares derivam dos automatos finitos da teoria dos formalismos gramaticais, conceito criado por Stephen Kleene na década de 50. Em Clojure, além das funções re-seq e re-matches, você também pode usar re-find para encontrar a primeira correspondência ou re-pattern para criar um padrão.

Entretanto, esteja ciente de que elas não são sempre a melhor solução. Em situações complexas, é recomendado utilizar uma verdadeira análise textual (parsing) ou uma biblioteca especializada.

## Veja Também

Para se aprofundar no assunto, sugerimos os seguintes recursos:

- Documentação oficial de expressões regulares em Clojure: [cljdoc.org](https://cljdoc.org/d/clojure/clojure/1.10.3/doc/core-functions#re-seq)
- Um guia detalhado e interativo sobre expressões regulares: [regex101.com](https://regex101.com/)
- Para problemas mais complicados, considere bibliotecas como: [instaparse](https://github.com/Engelberg/instaparse) e [parsec](https://github.com/satta/parsec)