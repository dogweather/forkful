---
title:                "Capitalizando uma string"
html_title:           "Clojure: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O Quê e Por Quê?

Capitalizar uma string consiste em alterar a primeira letra de cada palavra da string para maiúscula. Os programadores fazem isso para melhorar a aparência dos textos em suas aplicações, aumentando a legibilidade e a consistência do conteúdo.

## Como fazer:

```clojure
(defn capitlizar [s]
  (apply str
    (interleave (string/capitalize s) " ")))

(capitlizar "fazendo uma string em caixa alta")
;; "Fazendo Uma String Em Caixa Alta"
```

## Mergulho Profundo

Historicamente, capitalizar strings é uma prática que vem desde os primeiros sistemas de processamento de texto. No Clojure, como vimos, usamos a função `string/capitalize`.

No entanto, existem alternativas. Uma delas é usar a função `clojure.string/capitalize`, que capitaliza a primeira letra da string e transforma o restante em caixa baixa.

```clojure
(clojure.string/capitalize "FAZENDO uma STRING em caixa ALTA")
;; "Fazendo uma string em caixa alta"
```

Isso pode ser útil quando precisamos garantir que todas as outras letras da string estejam em minúsculas.

Sobre a implementação, o Clojure na verdade usa as funções de string do Java subjacente. Portanto a função `string/capitalize` é eficiente e bem testada, já que tem acesso à bem estabelecida API de String do Java.

## Veja Também

Para mais informações sobre manipulação de strings em Clojure, consulte o seguinte:
- Documentação oficial da Clojure para strings: https://clojuredocs.org/clojure.string
- Guia de Clojure sobre strings (em inglês): https://learnxinyminutes.com/docs/clojure/
- Página da Wikipedia sobre capitalização: https://pt.wikipedia.org/wiki/Capitalização