---
date: 2024-01-20 17:38:04.842497-07:00
description: "Como fazer: Historicamente, a necessidade de converter texto para um\
  \ caso uniforme existe desde os primeiros dias da computa\xE7\xE3o para simplificar\
  \ a\u2026"
lastmod: '2024-04-05T21:53:46.506809-06:00'
model: gpt-4-1106-preview
summary: "Historicamente, a necessidade de converter texto para um caso uniforme existe\
  \ desde os primeiros dias da computa\xE7\xE3o para simplificar a compara\xE7\xE3\
  o de strings."
title: "Convertendo uma string para min\xFAsculas"
weight: 4
---

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
