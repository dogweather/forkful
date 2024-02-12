---
title:                "Baixando uma página da web"
aliases:
- /pt/clojure/downloading-a-web-page.md
date:                  2024-01-20T17:43:48.609855-07:00
model:                 gpt-4-1106-preview
simple_title:         "Baixando uma página da web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Baixar uma página da web é simplesmente trazer conteúdo da internet para o seu computador. Programadores fazem isso para coletar dados, automatizar testes, ou integrar serviços.

## Como Fazer:
Vamos usar a biblioteca `clj-http` para baixar uma página:

```clojure
(require '[clj-http.client :as client])

(defn download-page [url]
  (client/get url))

;; Uso
(def page (download-page "http://example.com"))
(println (:status page))  ; Status code da resposta HTTP
(println (:body page))    ; Conteúdo da página
```

Isso vai imprimir algo como:

```
200
<!doctype html>...
```

## Mergulho Profundo:
Baixar páginas da web é uma prática que remonta aos primórdios da internet, onde a linha de comando ou simples scripts faziam o trabalho. Alternativas ao `clj-http` incluem `http-kit` ou ferramentas Java interop como `Jsoup` para parsing de HTML. Em termos de implementação, `clj-http` faz uso de abstrações baseadas em Java e pode integrar-se perfeitamente com outras bibliotecas Java para expandir funcionalidades como parsing e manipulação de cookies.

## Veja Também:
- Documentação `clj-http`: [https://github.com/dakrone/clj-http](https://github.com/dakrone/clj-http)
- `http-kit`: [http://www.http-kit.org/](http://www.http-kit.org/)
- `Jsoup`: [https://jsoup.org/](https://jsoup.org/)
- Clojure for the Brave and True (para aprofundar em Clojure): [https://www.braveclojure.com/](https://www.braveclojure.com/)
