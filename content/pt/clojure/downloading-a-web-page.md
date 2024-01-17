---
title:                "Baixando uma página da web"
html_title:           "Clojure: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O que é & Por que?
Baixar uma página da web é o ato de transferir o conteúdo de uma página da web armazenada em um servidor para o seu dispositivo local. Programadores geralmente fazem isso para obter informações ou dados de uma página da web, que podem ser usados em outros programas ou projetos.

## Como fazer:
```Clojure
(require '[clj-http.client :as client]) ;; importar a biblioteca HTTP
(def response (client/get "https://exemplo.com")) ;; atribuir a resposta do servidor à variável "response"
(:body response) ;; imprimir o conteúdo da página
```
    <h1>Bem-vindo ao Exemplo.com!</h1>
    <p>Aqui você pode encontrar informações e recursos úteis.</p>

## Profundando:
Baixar páginas da web é uma função essencial para muitos aplicativos e ferramentas em diferentes áreas, desde web scraping para coleta de dados até automação de tarefas em desenvolvimento. Além da biblioteca clj-http, existem outras alternativas, como o Java interop ou bibliotecas mais específicas, como o webclient do Clojure. Para baixar páginas com conteúdos dinâmicos, pode ser necessário usar ferramentas adicionais, como o Selenium ou PhantomJS.

## Veja também:
- [Documentação oficial do clj-http](https://github.com/dakrone/clj-http)
- [Java interop no Clojure](https://clojure.org/reference/java_interop)
- [Webclient no Clojure](https://github.com/hozza/clj-webclient)