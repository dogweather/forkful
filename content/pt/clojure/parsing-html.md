---
title:                "Análise de HTML"
date:                  2024-01-20T15:30:49.095351-07:00
html_title:           "Bash: Análise de HTML"
simple_title:         "Análise de HTML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?
Analisar HTML consiste em transformar o conteúdo marcado de uma página web em dados compreensíveis e manipuláveis. Programadores fazem isso para extrair informação, automatizar interações com websites ou testar conteúdos web.

## Como Fazer:
```Clojure
;; Adicione a biblioteca Enlive ao seu projeto
(require '[net.cgrand.enlive-html :as html])

;; Defina a URL e pegue o HTML dela
(def url "http://example.com")
(def page-html (html/html-resource (java.net.URL. url)))

;; Extrair títulos usando Enlive
(defn get-titles [html]
  (map :content (html/select html [:title])))

;; Uso da função
(println (get-titles page-html))
```
Saída de exemplo:
```
("Este é o título da página de exemplo")
```

## Aprofundamento
Analisar HTML é uma tática usada desde o início da web. Inicialmente, as análises eram mais rudimentares, muitas vezes, usando expressões regulares. Hoje, têm-se bibliotecas especializadas como Enlive ou Jsoup (em Java), que entendem a estrutura do HTML e oferecem maneiras mais confiáveis e flexíveis de acessar os dados.

Enlive, por exemplo, não altera o documento durante a análise, o que significa que o seu HTML não será "arruinado" durante o processo. Alternativamente, para tarefas mais complexas ou manipulação de DOM, algo como HtmlUnit pode ser mais apropriado, mas tem o custo de maior sobrecarga.

## Veja Também
- Documentação do Enlive: [https://github.com/cgrand/enlive](https://github.com/cgrand/enlive)
- Jsoup, uma alternativa Java: [https://jsoup.org/](https://jsoup.org/)
- HtmlUnit, para simular um navegador: [http://htmlunit.sourceforge.net/](http://htmlunit.sourceforge.net/)
