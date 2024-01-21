---
title:                "Convertendo uma string para minúsculas"
date:                  2024-01-20T17:38:04.842497-07:00
model:                 gpt-4-1106-preview
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Converter uma string para minúsculas significa transformar todos os caracteres maiúsculos em suas correspondentes minúsculas. Programadores fazem isso para padronizar dados, facilitar comparações de texto e evitar erros sensíveis a maiúsculas.

## Como fazer:
```Clojure
;; Utilizando a função `clojure.string/lower-case` para converter uma string para minúsculas
(require '[clojure.string :as str])

(defn string-para-minusculas [texto]
  (str/lower-case texto))

;; Exemplo de uso:
(println (string-para-minusculas "Olá, Mundo!")) ;=> "olá, mundo!"
```

## Mergulho Profundo
Historicamente, a necessidade de converter texto para um caso uniforme existe desde os primeiros dias da computação para simplificar a comparação de strings. Em Clojure, a função `clojure.string/lower-case` é a maneira padrão de realizar essa tarefa. Alternativas incluem o uso de Java interop (`(.toLowerCase texto)`), embora seja menos idiomático. A implementação interna da função lida com o padrão Unicode, garantindo que a conversão de maiúsculas para minúsculas funcione corretamente para um espectro amplo de caracteres de diferentes idiomas e scripts.

## Veja Também
- [ClojureDocs String Functions](https://clojuredocs.org/clojure.string)
- [Clojure String API](https://clojure.github.io/clojure/clojure.string-api.html)
- [Unicode Standard on Case](https://www.unicode.org/standard/standard.html)