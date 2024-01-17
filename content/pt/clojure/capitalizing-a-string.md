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

## O que & Por quê?
Capitalizar uma string é simplesmente transformar a primeira letra em maiúscula e o resto em minúsculas. Os programadores geralmente fazem isso para melhorar a aparência de uma string, tornando-a mais legível ou identificar palavras importantes em uma frase.

## Como fazer:
```Clojure
(clojure.string/capitalize "clojure é incrível") ; retorna "Clojure é incrível"
(clojure.string/capitalize "capítulo um: introdução") ; retorna "Capítulo um: introdução"
```

## Profundidade:
Capitalizar strings tem sido uma prática comum por muito tempo, especialmente em linguagens de programação mais antigas onde as strings eram às vezes limitadas em termos de caracteres. Algumas alternativas incluem usar a função `upper-case` e `lower-case` para transformar todas as letras em maiúsculas ou minúsculas, respectivamente. A implementação por trás da função `capitalize` usa a função `clojure.string/upper-case` para transformar a primeira letra em maiúscula e `clojure.string/lower-case` para o resto das letras em minúsculas.

## Veja também:
- [Documentação oficial do Clojure string module](https://clojure.github.io/clojure/clojure.string-api.html)
- [Pergunta sobre capitalizar strings no Stack Overflow](https://stackoverflow.com/questions/15601362/most-efficient-way-to-capitalize-a-string-in-clojure)