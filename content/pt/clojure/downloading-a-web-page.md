---
title:                "Clojure: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por que fazer o download de uma página da web?

Fazer o download de uma página da web pode ser útil em diversas situações, como por exemplo para extrair informações de um site ou para criar backups. Além disso, é uma ótima maneira de aprender e praticar programação em Clojure.

## Como fazer o download de uma página da web?

Fazer o download de uma página da web em Clojure é simples e fácil. Basta utilizar a função `slurp` que faz parte da biblioteca `clojure.java.io`. Veja um exemplo abaixo:

```Clojure
(require '[clojure.java.io :as io])

(def url "https://example.com")

(def page (slurp url))
```

Ao executar esse código, a variável `page` irá conter o conteúdo da página da web especificada na variável `url`.

Para salvar o conteúdo em um arquivo, podemos utilizar a função `spit` também da biblioteca `clojure.java.io`. Veja um exemplo abaixo:

```Clojure
(spit "page.html" page)
```

Isso irá criar um arquivo chamado `page.html` contendo o conteúdo da página baixada.

## Aprofundando-se no download de páginas da web

Além do `slurp` e `spit`, existem outras funções úteis para realizar o download de páginas da web em Clojure. Algumas delas são:

- `clojure.xml/parse`: função utilizada para analisar o conteúdo de uma página em formato XML e retornar um mapa com a estrutura do documento.
- `enlive-html/emit`: função que transforma um mapa do formato retornado pelo `clojure.xml/parse` em uma string contendo o conteúdo da página.
- `clojure.string/split`: função utilizada para separar uma string em partes, utilizando um determinado caractere como separador. Isso pode ser útil para obter informações específicas de uma página da web.
- `clojure.string/trim`: função que remove os espaços em branco no início e no final de uma string.
- `enlive-html/html-resource`: função que recebe uma URL como parâmetro e retorna uma representação de uma página HTML que pode ser manipulada utilizando as funções da biblioteca `enlive`.

## Veja também

- [Documentação da biblioteca clojure.java.io](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [Documentação da biblioteca enlive](https://www.luminusweb.net/docs/library/enlive.html)
- [Tutorial de Clojure para iniciantes](https://www.luminusweb.net/docs/tutorials/clojure.html)