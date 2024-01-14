---
title:                "Clojure: Procurando e substituindo texto"
simple_title:         "Procurando e substituindo texto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Por que fazer substituições de texto em Clojure

Fazer substituições de texto é uma tarefa comum em programação, seja para corrigir erros ou para transformar dados de uma forma específica. Em Clojure, existem várias maneiras de realizar essa tarefa, o que torna a linguagem uma ótima opção para quem precisa lidar com manipulação de texto.

## Como fazer substituições de texto em Clojure

Existem duas principais funções em Clojure para substituir texto: `replace` e `replace-first`. Ambas recebem como parâmetros uma expressão regular e a string de substituição. Vamos ver como elas funcionam em um exemplo:

```Clojure
(println (replace #"mundo" "Hello mundo" "World"))
; Saída: Hello World
(println (replace-first #"\d+" "Eu tenho 2 maçãs e 3 bananas"))
; Saída: Eu tenho maçãs e 3 bananas
```

No primeiro exemplo, usamos a função `replace` para substituir a palavra "mundo" por "World" na string "Hello mundo". Já no segundo exemplo, usamos a função `replace-first` para substituir o primeiro número (\d+) por uma string vazia, removendo-o da frase.

## Mais informações sobre substituições de texto em Clojure

Em Clojure, as expressões regulares são escritas entre aspas duplas e precedidas por um `#`. Além disso, a função `replace` substitui todas as ocorrências da expressão regular, enquanto a função `replace-first` substitui apenas a primeira ocorrência.

Outra funcionalidade interessante é a possibilidade de usar grupos na expressão regular e referenciá-los na string de substituição. Para isso, basta utilizar `\1`, `\2`, etc. para substituir o primeiro, segundo, etc. grupo.

## Veja também

- Documentação oficial do Clojure para substituições de texto: https://clojuredocs.org/clojure.core/replace
- Tutorial sobre substituições de texto em Clojure: https://www.baeldung.com/clojure/regex-replace