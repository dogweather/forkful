---
title:                "Clojure: Excluindo caracteres correspondentes a um padrão"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Quando estamos trabalhando em um projeto de programação, muitas vezes encontramos situações em que precisamos deletar certos caracteres de uma string que correspondem a um certo padrão. Isso pode ser útil para processar dados, reformatar texto ou até mesmo limpar dados de entrada. Em Clojure, existem algumas maneiras diferentes de realizar essa tarefa.

## Como Fazer

Você pode usar a função `replace` para deletar caracteres que correspondem a um padrão em uma string. Por exemplo, se quisermos deletar todas as ocorrências de números de uma string, podemos utilizar a seguinte expressão:

```Clojure
(replace "123abc456def" #"[0-9]" "")
; => "abcdef"
```

Nesse caso, a função `replace` recebe três parâmetros: a string original, um padrão regular expressão que representa os caracteres que queremos deletar (no caso, qualquer dígito) e uma string vazia como terceiro parâmetro, indicando que queremos substituir esses caracteres por nada.

Uma outra maneira de deletar caracteres que correspondem a um padrão é utilizando a função `replace-first`, que só irá deletar a primeira ocorrência do padrão. Porém, essa função também aceita um segundo parâmetro opcional, que especifíca por quantas substituições queremos fazer. Por exemplo:

```Clojure
(replace-first "abc123def456" #"[0-9]" "" 2)
; => "abcdef456"
```

Nesse caso, apenas as duas primeiras ocorrências de dígitos são removidas da string.

## Deep Dive

Além das funções `replace` e `replace-first`, Clojure também possui outras opções para deletar caracteres que correspondem a um padrão. A função `subs` pode ser utilizada para retornar uma parte de uma string, eliminando os caracteres que correspondem ao padrão especificado. Por exemplo:

```Clojure
(subs "abc123def456" #"[0-9]")
; => "abcdef"
```

Outra opção é utilizar a função `re-seq`, que irá retornar uma seqüência de todas as ocorrências dos caracteres que correspondem ao padrão. Por exemplo:

```Clojure
(re-seq #"[0-9]" "abc123def456")
; => ("1" "2" "3" "4" "5" "6")
```

## Veja Também

Se você quiser aprender mais sobre as diversas funções relacionadas a strings em Clojure, confira a documentação oficial em https://clojure.org/api/string ou dê uma olhada nesses tutoriais: 

- https://docs.oracle.com/javase/7/docs/api/java/lang/String.html
- https://www.braveclojure.com/do-things/
- https://cljdoc.org/d/clojure/clojure/1.10.0-alpha5/api/clojure.string#format