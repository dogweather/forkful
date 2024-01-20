---
title:                "Analisando HTML"
html_title:           "Arduino: Analisando HTML"
simple_title:         "Analisando HTML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Analisar HTML, ou "parsing", permite extrair dados de uma página web. Programadores fazem isso para coletar informações, automatizar tarefas, entre outras coisas.

## Como Fazer:
Clojure fornece diversas bibliotecas para análise HTML. Aqui, mostramos como fazê-lo usando a biblioteca `hickory`.

Para instalar o hickory, adicione a seguinte linha em seu arquivo `project.clj`:
```clojure
[hickory "0.7.1"]
```

Vamos começar com um exemplo simples:

```clojure
(ns exemplo.core
  (:require [hickory.core :as h]))

(defn parse-html [html]
  (->> html
       h/as-hickory))

(defn main [& args]
  (println (parse-html "<html><body><h1>Oi, mundo!</h1></body></html>")))
```
A saída será:
```clojure
{:tag :html, :attrs {}, :content [{:tag :body, :attrs {}, :content [{:tag :h1, :attrs {}, :content ["Oi, mundo!"]}]}]}
```

## Mergulho Profundo
A análise HTML vem sendo uma parte crucial da programação na web desde seus primeiros dias. No entanto, as bibliotecas modernas permitem fazê-lo de forma mais eficiente e segura.

Alternativas ao `hickory` incluem `clojure.data.xml` e `enlive`.

Nas entranhas, o `hickory` usa a biblioteca Java `jsoup` para converter HTML em um formato interpretável.

## Veja Também
1. Documentação do Hickory: [https://github.com/davidsantiago/hickory](https://github.com/davidsantiago/hickory)
2. Um tutorial sobre análise HTML com Clojure: [https://www.tutorialspoint.com/clojure/clojure_web_programming.htm](https://www.tutorialspoint.com/clojure/clojure_web_programming.htm)
3. Documentação do jsoup: [https://jsoup.org/](https://jsoup.org/)