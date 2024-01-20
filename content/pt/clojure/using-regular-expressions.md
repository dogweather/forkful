---
title:                "Utilizando expressões regulares"
html_title:           "Bash: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?
Usar expressões regulares é como brincar de detetive com texto: você define padrões para encontrar ou manipular pedaços específicos de strings. Programadores utilizam porque é uma ferramenta poderosa para validação, extração e substituição de informações em textos complexos com precisão e eficiência.

## Como Fazer:
```Clojure
;; Verificar se um string atende a uma expressão regular
(re-matches #"\d+" "123") ; => "123"

;; Encontrar todas as ocorrências de um padrão em um texto
(re-seq #"\w+" "Olá mundo 123!") ; => ("Olá" "mundo" "123")

;; Substituir partes de um string que atendam a um padrão
(clojure.string/replace "15/04/2023" #"\d{2}/\d{2}/" "01/01/") ; => "01/01/2023"
```

## Aprofundando
Expressões regulares não são uma invenção recente; surgiram na década de 1950 com o trabalho do matemático Stephen Kleene. Como alternativas, linguagens oferecem a busca de strings e funções de parse, mas nada supera a expressão regular em flexibilidade. A implementação em Clojure usa o Java `java.util.regex`, que é parte do Java Standard Library, garantindo bom desempenho e confiabilidade.

## Veja Também
- [ClojureDocs - Regular Expressions](https://clojuredocs.org/clojure.string/replace)
- [Clojure from the ground up: regex](https://aphyr.com/posts/305-clojure-from-the-ground-up-regex)
- [Java Pattern Class](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)