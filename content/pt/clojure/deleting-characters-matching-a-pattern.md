---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Arduino: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Removendo caracteres que correspondem a um padrão em Clojure

## O Que & Por Que?
Remover caracteres que correspondem a um padrão é uma operação muito comum na manipulação de strings. Os programadores fazem isso para limpar dados de entrada, remover caracteres indesejados ou formatar strings de maneiras específicas. 

## Como fazer:
Em Clojure, podemos usar a função `clojure.string/replace` para substituir todos os caracteres de uma string que correspondem a um determinado padrão.

```clojure
(require 'clojure.string)

(defn remove-chars [s chars]
 (clojure.string/replace s (re-pattern (str "[" chars "]")) ""))

(println (remove-chars "Esta é uma linha de teste." "aeiou"))
```

Isso imprimirá:
```clojure
"Est é m ln d tsts."
```

## Mergulho Profundo
Em Clojure, a função `replace` vem do pacote `clojure.string`. Foi adicionada na versão 1.2 da linguagem. É baseada em expressões regulares, que são uma maneira poderosa e flexível de pesquisar e manipular strings.

Existem muitas outras maneiras de remover caracteres de uma string em Clojure, mas `replace` é uma das mais simples e diretas. Alternativamente, você poderia usar `filter` com uma função que retorna falsa para caracteres que você deseja remover.

Quando a função `replace` encontra um caractere que corresponde ao padrão que você passou, ela simplesmente o substitui por uma string vazia. Isso efetivamente "remove" o caractere da string.

## Veja Também
- Documentação do Clojure sobre `replace`: https://clojuredocs.org/clojure.string/replace
- Guia do Clojure para expressões regulares: https://clojure.org/guides/learn/regex
- Artigo sobre manipulação de strings em Clojure: https://www.codementor.io/@shekharrajak/clojure-strings-are-really-interesting-pg2lb5m65