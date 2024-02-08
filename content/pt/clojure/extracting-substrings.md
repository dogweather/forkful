---
title:                "Extraindo substrings"
aliases:
- pt/clojure/extracting-substrings.md
date:                  2024-01-20T17:45:16.911728-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extraindo substrings"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## O Que É & Porquê?
Extrair substrings é uma maneira de pegar pedaços de uma string — como pegar só o nome de alguém numa frase de boas-vindas. Programadores fazem isso quando querem manipular ou analisar apenas uma parte específica de um texto.

## Como Fazer:
Com Clojure, extraímos substrings com a função `subs`. A função requer a string original e o índice inicial (incluído). Um índice final (não incluído) é opcional. Veja só:

```Clojure
(def saudacao "Olá, João! Bem-vindo ao clube.")

;; Extrair o nome "João"
(println (subs saudacao 5 9))
;; Saída esperada: "João"

;; Extrair sem especificar o índice final pega tudo até o fim
(println (subs saudacao 10))
;; Saída esperada: "Bem-vindo ao clube."
```
`subs` é direto: dê o que começar e onde terminar, e ele faz o serviço.

## Mergulhando Fundo
A função `subs` vem da longa tradição de linguagens de programação que trabalham com strings - extração de substring é uma necessidade comum. Em Clojure, que é uma linguagem moderna na família Lisp, `subs` é a ferramenta básica para isso, mas verificando-se a imutabilidade das strings em Clojure, qualquer extração resulta em uma nova string.

Existem alternativas em Clojure, como usar regex (expressões regulares) com `re-find` e `re-matches` ou manipulações em nível de coleção com o `take` e `drop`. Escolhemos a ferramenta dependendo da complexidade da nossa necessidade.

Particularmente, `subs` funciona bem para casos simples e claros. Já regex é a melhor opção para padrões complexos. Como em todas as operações de string, temos que lidar com a indexação baseada em zero — o primeiro elemento é indexado por 0.

## Veja Também
- Documentação oficial de Clojure [subs](https://clojuredocs.org/clojure.core/subs)
- Um guia para regex em Clojure: [Clojure - Regular Expressions](https://www.tutorialspoint.com/clojure/clojure_regular_expressions.htm)
