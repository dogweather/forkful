---
title:    "Clojure: Excluindo caracteres que correspondem a um padrão"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Por que
Excluir caracteres que correspondem a um padrão pode ser útil em situações em que você precisa limpar uma string ou arquivo de texto, removendo espaços em branco ou caracteres específicos.

## Como Fazer
Para excluir caracteres correspondentes a um padrão em Clojure, podemos usar a função `clojure.string/replace` combinada com uma expressão regular. Veja um exemplo de código abaixo:

```Clojure
(require '[clojure.string :as str])

(def texto "Olá mundo!")

(str/replace texto #"mundo" "")
```

A saída desse código seria a string "Olá !", onde o trecho "mundo" foi removido.

## Aprofundando
É importante entender como as expressões regulares funcionam para usar esse método corretamente. Expressões regulares são padrões usados para encontrar combinações de caracteres em uma string. No exemplo acima, usamos a expressão regular `#"mundo"`, que procura pelo trecho "mundo" na string fornecida e o substitui por uma string vazia, efetivamente removendo-o.

Além disso, podemos usar a função `clojure.string/replace-first`, que apenas substitui o primeiro caractere correspondente ao padrão encontrado, em vez de substituir todas as ocorrências. Isso pode ser útil em algumas situações específicas.

## Veja também
- [Documentação oficial do clojure.string/replace](https://clojuredocs.org/clojure.string/replace)
- [Tutorial de expressões regulares em Clojure](https://clojure.org/guides/learn/regular_expressions)