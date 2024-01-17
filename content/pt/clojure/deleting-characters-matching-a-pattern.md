---
title:                "Excluindo caracteres correspondentes a um padrão"
html_title:           "Clojure: Excluindo caracteres correspondentes a um padrão"
simple_title:         "Excluindo caracteres correspondentes a um padrão"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O que & Por quê? 
Deletar caracteres que correspondem a um padrão é um processo comum em programação, onde queremos remover um determinado conjunto de caracteres de uma determinada string ou seqüência de caracteres. Isso pode ser útil quando queremos limpar ou filtrar dados ou quando precisamos criar um formato específico para uma string.

## Como: 
Nós podemos usar a função `replace` para excluir caracteres que correspondam a um padrão de uma string ou seqüência de caracteres. Veja o exemplo abaixo:

```Clojure
(replace #"\d" "abc123def")
```

O código acima irá retornar a string `"abcdef"`, pois ele removeu todos os caracteres numéricos da string original. 

## Mergulho Profundo: 
Antes de `Clojure` existir, a linguagem `Perl` já tinha uma função chamada `s///` que fazia exatamente isso e serviu de inspiração para a função `replace` em `Clojure`. Existem outras maneiras de realizar essa mesma tarefa em `Clojure`, como usar a função `str/replace` do namespace `clojure.string` ou usar expressões regulares mais avançadas.

## Veja Também: 
Para saber mais sobre o uso da função `replace` em `Clojure`, consulte a documentação oficial: https://clojuredocs.org/clojure.string/replace. Você também pode explorar outras funções úteis do namespace `clojure.string`, como `split` e `join`.