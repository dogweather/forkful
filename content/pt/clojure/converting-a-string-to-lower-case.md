---
title:                "Convertendo uma string para minúsculas"
html_title:           "Clojure: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O que e Porque?
Conversão de uma string para letras minúsculas significa transformar todas as letras maiúsculas em letras minúsculas. Isso é feito para fins de padronização e facilitar a comparação de strings por programas ou algoritmos. 

## Como fazer:
```Clojure
;; Usando a função lower-case
(lower-case "TEXTO EM MAIÚSCULAS")
;; Saída: "texto em maiúsculas"

;; Usando a função to-lower-case do namespace clojure.string
(require '[clojure.string :as str])
(str/to-lower-case "OUTRA STRING MAIÚSCULA")
;; Saída: "outra string maiúscula"
```

## Mergulho Fundo:
A conversão de strings para letras minúsculas é uma técnica amplamente utilizada em programação, principalmente em linguagens funcionais como Clojure. Isso se deve ao fato de que transformar todos os caracteres de uma string em letras minúsculas é uma operação mais simples e eficiente do que buscar correspondências entre caracteres maiúsculos e minúsculos. Alternativas para essa operação incluem usar expressões regulares ou criar sua própria função de conversão. A implementação da função lower-case no Clojure é feita através de um mapeamento dos códigos ASCII dos caracteres. 

## Veja também:
- Documentação oficial do Clojure sobre a função lower-case: https://clojuredocs.org/clojure.core/lower-case
- Exemplo de uso da função lower-case: https://www.braveclojure.com/core-functions-in-depth/#Lowercase