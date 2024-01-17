---
title:                "Pesquisando e substituindo texto"
html_title:           "Clojure: Pesquisando e substituindo texto"
simple_title:         "Pesquisando e substituindo texto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O que é e por quê?

Procurar e substituir texto é uma tarefa comum para os programadores. Isso envolve encontrar e substituir uma determinada sequência de caracteres em uma string por outro conjunto de caracteres. Isso é útil quando você precisa fazer alterações em grande quantidade de texto ou código em um único comando.

Os programadores fazem isso para economizar tempo e minimizar erros em tarefas repetitivas de edição de texto. Isso também permite alterar rapidamente várias instâncias de um pedaço de código sem ter que fazer manualmente alterações em cada uma delas.

## Como fazer:

```Clojure
;; Para procurar e substituir texto em uma string, use a função "replace" com uma expressão regular e a string de substituição.
(replace #"antigo" "novo" "Este é um texto antigo.")

;; Output => "Este é um texto novo."
```

```Clojure
;; Você também pode procurar e substituir texto em arquivos usando o clojure.java.io namespace.
(require '[clojure.java.io :as io])

(with-open [in-file (io/reader "arquivo.txt")
            out-file (io/writer "arquivo_novo.txt")]
    (replace #"antigo" "novo" in-file out-file))

;; Este exemplo substitui todas as ocorrências de "antigo" por "novo" no arquivo "arquivo.txt" e armazena o resultado em um novo arquivo chamado "arquivo_novo.txt".
```

## Profundando:

Substituir texto em arquivos é uma tarefa comum em linguagens de programação e há diferentes abordagens para isso. Alguns programadores preferem usar um editor de texto com recursos de busca e substituição, enquanto outros optam por usar bibliotecas ou ferramentas especializadas para isso.

Os programadores também podem encontrar expressões regulares (também conhecidas como "regex") úteis para procurar e substituir padrões de texto complexos. No entanto, é importante ter cuidado ao usar expressões regulares, pois elas podem ser facilmente mal interpretadas ou mal utilizadas.

## Veja também:

- [Documentação oficial do Clojure sobre funções de strings](https://clojuredocs.org/clojure.core/replace)
- [Guias para expressões regulares em Clojure](https://www.lispcast.com/reg-ex)
- [Ferramenta gratuita para testar expressões regulares em tempo real](https://regexr.com/)