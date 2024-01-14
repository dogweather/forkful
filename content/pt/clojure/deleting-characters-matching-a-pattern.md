---
title:                "Clojure: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Por que deletar caracteres que correspondem a um padrão?

Existem algumas situações em que é necessário eliminar caracteres que correspondem a um determinado padrão em uma string. Isso pode ser útil, por exemplo, quando se deseja limpar uma string de caracteres indesejados antes de realizar alguma operação com ela.

## Como fazer

Em Clojure, podemos usar a função `replace` para substituir caracteres que correspondem a um padrão por uma string vazia. Vamos dar uma olhada em um exemplo de código:

````Clojure
(def string-a-limpar "Olá, (mundo)!")

(replace #"[(),!]" "" string-a-limpar)

; Saída:
; "Olá mundo"
````

Neste exemplo, estamos usando a expressão regular `#[(),!]` para representar o padrão que queremos deletar. Note que os caracteres dentro dos colchetes correspondem aos caracteres que queremos eliminar, e as aspas duplas representam a string vazia que será usada para substituí-los. O terceiro parâmetro da função `replace` é a string que queremos limpar.

## Mergulho profundo

Além da função `replace`, também existem outras maneiras de deletar caracteres que correspondem a um padrão em Clojure. Podemos usar a função `clojure.string/replace` ou a função `subs`, que nos permite substituir uma parte de uma string por outra.

Também é possível usar expressões regulares mais complexas, com grupos de captura, para deletar apenas partes específicas da string correspondentes a determinados padrões. Essas ferramentas podem ser úteis em situações mais avançadas em que é necessário um controle mais preciso sobre a remoção de caracteres.

# Veja também

- [Documentação oficial de Clojure sobre expressões regulares](https://clojure.org/reference/regular_expressions) 
- [Tutorial sobre expressões regulares em Clojure](https://tech.bigcartel.com/blog/regular-expressions-in-clojure) 
- [Exemplos de uso de expressões regulares em Clojure](https://www.jayway.com/2014/06/14/clojure-regular-expressions/)