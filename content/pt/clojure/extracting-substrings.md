---
title:                "Extraindo substrings"
html_title:           "Bash: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

# Extração de Substrings em Clojure: Uma Abordagem Direta

## O Que & Porquê?

A extração de substrings é o processo de puxar uma sequência específica de caracteres de uma string maior. Os programadores fazem isso para manipular e analisar dados mais eficientemente.

## Como Fazer:

As seguintes funções em Clojure podem ser usadas para extrair substrings:

```clojure
;; Sintaxe básica
(subs "string" start-index end-index)

;; Exemplo
(def exemplo "Programação em Clojure")

;; Usando a função 'subs'
(subs exemplo 0 12) ;; Retorna "Programação"
(subs exemplo 13) ;; Retorna "em Clojure"
```

Lembre-se de que a indexação em Clojure (como na maioria das linguagens de programação) começa a partir de 0.

## Mergulho Profundo

Clojure, sendo um dialeto de Lisp, usa muitos dos padrões antigos bem consolidados enquanto adiciona suas próprias melhorias onde apropriado. A função 'subs', por exemplo, utiliza a abordagem clássica de indexação zero baseada em intervalos semiabertos (o índice de início está incluído, mas o de fim não).

Como alternativa, Clojure também fornece formas mais sofisticadas de lidar com strings através de expressões regulares, tal como a função 're-seq'. A contudo lembrar, o uso cuidadoso de expressões regulares pode levar a resultados mais potentes, mas também mais complexos.

```clojure
;; Exemplo
(def exemplo "Programação em Clojure")

;; Usando a função 're-seq'
(re-seq #"\w+" exemplo) ;; Retorna ["Programação" "em" "Clojure"]
```

## Veja Também

Para mais detalhes e para se aprofundar mais na extração e manipulação de strings em Clojure, consulte os seguintes recursos:

2. [Clojure String API - Documentação oficial](https://clojuredocs.org/clojure.string)