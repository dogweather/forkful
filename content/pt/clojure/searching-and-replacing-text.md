---
title:                "Buscando e substituindo texto"
html_title:           "Clojure: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

A substituição de texto é uma tarefa comum ao trabalhar com texto em programação. Pode ser necessário alterar nomes de variáveis, corrigir erros de digitação ou fazer alterações em massa em um grande arquivo de texto. A capacidade de efetivamente pesquisar e substituir texto é uma habilidade essencial para melhorar a produtividade e a eficiência na codificação.

## Como fazer

A substituição de texto em Clojure é realizada usando a função `replace` do namespace `clojure.string`. Esta função leva três argumentos: a string original, o texto a ser pesquisado e o texto de substituição. Aqui está um exemplo de como usar a função `replace` para substituir todas as instâncias do texto "apple" por "banana" em uma string:

```Clojure
(require '[clojure.string :as str])

(str/replace "I love apple pie" "apple" "banana")
```

A saída desse código seria `"I love banana pie"`, mostrando que a função `replace` substituiu com sucesso o texto desejado. Além disso, também é possível usar expressões regulares para uma substituição mais sofisticada. Aqui está um exemplo de como substituir todas as letras maiúsculas por letras minúsculas:

```Clojure
(str/replace "Hello WORLD" #"[A-Z]" (fn [m] (str/upper-case m)))
```

Este código produziria a saída `"hello world"`, pois a função de substituição usa uma expressão lambda para converter todas as letras maiúsculas em suas equivalentes minúsculas.

## Mergulho Profundo

A função `replace` é útil para substituições simples, mas existem outras funções em Clojure que podem ser usadas para casos mais complexos. A função `replace-first` substitui apenas a primeira ocorrência do texto pesquisado, enquanto a função `replace-nth` substitui a n-ésima ocorrência. É importante notar que todas essas funções retornam uma nova string, em vez de modificar a string original.

Outra opção é usar a função `re-seq`, que retorna um sequenciador de todas as correspondências do texto pesquisado. Isso pode ser útil para situações em que você precisa substituir apenas determinadas ocorrências. Por exemplo, você pode usar `re-seq` em conjunto com `replace-nth` para substituir apenas a segunda ocorrência de um texto.

## Veja também

- [Documentação oficial do Clojure](https://clojure.org/)
- [Tutorial de Clojure do "Learn X in Y Minutes"](https://learnxinyminutes.com/docs/clojure-pt/)