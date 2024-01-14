---
title:    "Clojure: Convertendo uma data em uma string."
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que

Converter uma data para uma string é uma tarefa comum em programação. Isso pode ser útil para exibir a data em um formato legível para humanos, armazenar em um banco de dados ou gerar relatórios.

## Como fazer

Existem várias maneiras de converter uma data para uma string em Clojure. Aqui estão alguns exemplos usando a biblioteca "clj-time".

```
(require '[clj-time.core :refer [now]])

(def current-date (now)) ;; cria um objeto de data atual

;; Converter para string no formato "yyyy-MM-dd"
(require '[clj-time.format :as f])
(f/unparse (f/formatters :year-month) current-date) ;; => "2021-04-19"

;; Converter para string no formato "dd/MM/yyyy"
(f/unparse (f/formatters :day-month-year) current-date) ;; => "19/04/2021"

;; Converter para string no formato "dd de MMMM de yyyy"
(require '[clj-time.format :as f]
  '[clj-time.coerce :as c])
(f/unparse (f/formatters "dd 'de' MMMM 'de' yyyy") (c/to-date "2021-04-19")) ;; => "19 de Abril de 2021"
```

## Mergulho Profundo

Ao converter uma data para uma string, é importante lembrar que cada formato de data tem um significado específico. Por exemplo, usar "yyyy-MM-dd" para representar uma data pode ser confuso para quem está acostumado com o formato "dd/MM/yyyy". Além disso, ao armazenar datas em um banco de dados, é recomendável usar um tipo de dado específico para datas em vez de strings.

Outro aspecto importante é a localização. Dependendo da linguagem do sistema e do idioma do usuário, as datas podem ser exibidas de maneiras diferentes. É importante considerar esses fatores ao converter uma data para uma string.

## Veja também

- [Documentação Clj-time] (https://clj-time.github.io/clj-time/doc/index.html)
- [Guia de estilo Clojure] (https://guide.clojure.style/#date-formatting)
- [Vídeo "Clojure Date Time Types and Formats"] (https://www.youtube.com/watch?v=thp0fJUr9n8)