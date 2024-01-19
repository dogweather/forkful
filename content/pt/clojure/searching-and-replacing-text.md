---
title:                "Pesquisando e substituindo texto"
html_title:           "Bash: Pesquisando e substituindo texto"
simple_title:         "Pesquisando e substituindo texto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
A procura e substituição de textos é uma operação comum no desenvolvimento de software, isso envolve identificar uma cadeia específica na entrada e substituí-la por outra. Os programadores fazem isso porque é uma maneira eficiente de alterar grandes volumes de informações mantendo a precisão.

## Como fazer:
O Clojure oferece várias funções úteis para manipulação de strings. Veja um exemplo de como usar a função replace:

```Clojure
(use 'clojure.string)
(replace "Hello, World!" "World" "Clojure")
```
Saída:
```Clojure
"Hello, Clojure!"
```
Neste exemplo, estamos substituindo "World" por "Clojure" na string "Hello, World!"

## Em Detalhes:
Na verdade, a função replace em Clojure substitui todas as ocorrências da seqüência de destino. Foi projetada desta maneira para uma substituição eficiente e global. Clojure, sendo uma linguagem Lisp, segura a tradição Lisp de imutabilidade e transformação de dados.

As alternativas para procurar e substituir texto em Clojure incluem funções como `replace-first` que substituirá apenas a primeira ocorrência do padrão de destino. Para mais controle, você pode usar uma função regex, como `re-seq`, para procurar padrões complexos.

A implementação real de replace no Clojure é, na verdade, baseada em regex e utiliza a implementação subjacente da JVM.

## Veja Também:
Para aprender mais sobre as funções de string Clojure, visite a documentação oficial [aqui](https://clojure.github.io/clojure/clojure.string-api.html).
Para mais informações sobre expressões regulares (ou regex), um bom recurso é o [Guia de Regex de Clojure](https://clojure.org/guides/learn/regex).